library(tidyverse)
library(rvest)
library(httr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0'
)

get_betrivers_page <- function(event_id){
  
  Sys.sleep(2)
  res <- httr::GET(url = glue::glue("https://eu-offering.kambicdn.org/offering/v2018/rsiusil/betoffer/event/{event_id}.json?lang=en_US&market=US-IL&client_id=2&channel_id=1&ncid=1669220664961&includeParticipants=true"),
                   httr::add_headers(.headers=headers))
  
  json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))
  
  return(json_obj[["betOffers"]])
  
}

get_betrivers_events <- function(page_number){
  
  res <- httr::GET(url = glue::glue("https://il.betrivers.com/api/service/sportsbook/offering/listview/events?t=20221181730&cageCode=847&type=live&type=prematch&groupId=1000093656&pageNr=1&pageSize=20&offset=0"),
                   httr::add_headers(.headers=headers))
  
  
  json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))
  
  return(json_obj[["items"]])
}

betrivers_props <- 
  tibble(page_number = 1) %>% 
  # tibble(page_number = c(1,2)) %>% 
  mutate(pages = map(.x = page_number, .f = get_betrivers_events)) %>% 
  unnest(pages) %>% 
  unnest_wider(pages) %>% 
  select(event_id = id) %>% 
  mutate(nested_markets = map(.x = event_id, .f = get_betrivers_page)) %>% 
  unnest(nested_markets)

betrivers_props_wide <- 
  betrivers_props %>% 
  unnest_wider(nested_markets) %>% 
  unnest(outcomes) %>% 
  unnest_wider(outcomes, names_sep = "_") %>%
  unnest_wider(criterion, names_sep = "_") %>% 
  separate(col = outcomes_participant, into = c("last_name", "first_name"), sep = "\\,\\s") %>% 
  filter(criterion_label %in% c("Total Rushing Yards by the Player",
                                "Alternate Rushing Yards by the Player - Including Overtime",
                                "Total Rushing Attempts by the Player",
                                "Total Rushing & Receiving Yards by the Player",
                                "Total Receptions by the Player",
                                "Total Receiving Yards by the Player",
                                "Total Passing Yards by the Player",
                                "Total Touchdown Passes Thrown by the Player",
                                "Alternate Passing Yards by the Player - Including Overtime",
                                "Total Pass Completions by the Player",
                                "TD Scorer",
                                "Player to score at least 2 touchdowns"),
         outcomes_status == "OPEN") %>%
  group_by(id) %>%
  fill(c(first_name, last_name), .direction = "updown") %>%
  ungroup() %>%
  transmute(
    market_site = "betrivers",
    merge_name = nflreadr::clean_player_names(paste(first_name, last_name)),
    market_name = case_when(criterion_label == "Total Rushing Yards by the Player" ~ "rushing_yards",
                            criterion_label == "Alternate Rushing Yards by the Player - Including Overtime" ~ "rushing_yards",
                            criterion_label == "Total Receiving Yards by the Player" ~ "receiving_yards",
                            criterion_label == "Total Receptions by the Player" ~ "receptions",
                            criterion_label == "Total Rushing & Receiving Yards by the Player" ~ "rushing_receiving_yards",
                            criterion_label == "Total Rushing Attempts by the Player" ~ "rushing_attempts",
                            criterion_label == "Total Passing Yards by the Player" ~ "passing_yards",
                            criterion_label == "Total Touchdown Passes Thrown by the Player" ~ "passing_touchdowns",
                            criterion_label == "Alternate Passing Yards by the Player - Including Overtime" ~ "passing_yards",
                            criterion_label == "Total Pass Completions by the Player" ~ "passing_completions",
                            criterion_label == "TD Scorer" ~ "rushing_receiving_touchdowns",
                            criterion_label == "Player to score at least 2 touchdowns" ~ "rushing_receiving_touchdowns",
                            TRUE ~ "error"),
    criterion_label,
    outcomes_label,
    outcomes_line,
    
    alt_flag = str_detect(criterion_label, "Alternate"),
    over_under = case_when(str_detect(outcomes_label, "Over") ~ "over",
                           str_detect(outcomes_label, "Yes") ~ "over",
                           str_detect(outcomes_label, "Under") ~ "under",
                           TRUE ~ "error"),
    market_odds_prob = odds.converter::odds.us2prob(as.numeric(outcomes_oddsAmerican)),
    market_line = case_when(criterion_label == "TD Scorer" ~ 0.5,
                            criterion_label == "Player to score at least 2 touchdowns" ~ 1.5,
                            alt_flag ~ parse_number(outcomes_label),
                            TRUE ~ outcomes_line / 1000)) %>% 
  pivot_wider(id_cols = c(market_site, merge_name, market_name, market_line, alt_flag),
              values_from = market_odds_prob,
              names_from = over_under,
              values_fn = min)

# betrivers_props_wide %>%
#   dplyr::group_by(market_site, merge_name, market_name, market_line, alt_flag, over_under) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)

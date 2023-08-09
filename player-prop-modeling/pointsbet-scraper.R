library(tidyverse)
library(rvest)
library(httr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0'
)

get_pointsbet_page <- function(event_id){
  
  Sys.sleep(2)
  res <- httr::GET(url = glue::glue('https://api.il.pointsbet.com/api/mes/v3/events/{event_id}'),
                   httr::add_headers(.headers=headers))
  
  json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))
  
  return(json_obj[["fixedOddsMarkets"]])
  
}

res <- httr::GET(url = "https://api.il.pointsbet.com/api/v2/competitions/2/events/featured?includeLive=false&page=1",
                 httr::add_headers(.headers=headers))


json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))

pointsbet_props <- 
  tibble(events = json_obj[["events"]]) %>% 
  unnest_wider(events) %>% 
  select(event_id = key) %>% 
  mutate(nested_markets = map(.x = event_id, .f = get_pointsbet_page)) %>% 
  unnest(nested_markets)

pointsbet_props_wide <- 
  pointsbet_props %>% 
  unnest_wider(nested_markets) %>% 
  unnest(outcomes) %>% 
  unnest_wider(outcomes, names_sep = "_") %>%
  filter(groupName %in% c("Rushing Props",
                          "Receiving Props",
                          "Quarterback Props",
                          # "Touchdowns",
                          "Player Props")) %>% 
  transmute(
    market_site = "pointsbet",
    merge_name = nflreadr::clean_player_names(str_extract(outcomes_name, "^(.+?)(?=(( To)|( Over)|( Under)))")),
    market_name = case_when(eventName == "Rushing Yards" ~ "rushing_yards",
                            eventName == "Running Back To Get" ~ "rushing_yards",
                            eventName == "Receiving Yards" ~ "receiving_yards",
                            eventName == "Receiver To Get" ~ "receiving_yards",
                            eventName == "Player Receptions" ~ "receptions",
                            eventName == "Quarterback Passing Yards" ~ "passing_yards",
                            eventName == "Quarterback Passing Touchdowns" ~ "passing_touchdowns",
                            eventName == "Quarterback Pass Attempts" ~ "passing_attempts",
                            eventName == "Quarterback Pass Completions" ~ "passing_completions",
                            eventName == "Quarterback To Get" ~ "passing_yards",
                            eventName == "Rushing + Receiving Yards" ~ "rushing_receiving_yards",
                            eventName == "Rushing Attempts Over/Under" ~ "rushing_attempts",
                            TRUE ~ "error"),
    alt_flag = str_detect(eventName, "To Get"),
    over_under = case_when(str_detect(outcomes_name, "Over") ~ "over",
                           str_detect(outcomes_name, "Under") ~ "under",
                           str_detect(outcomes_name, "To Get") ~ "over",
                           TRUE ~ "error"),
    market_odds_prob = odds.converter::odds.dec2prob(outcomes_price),
    market_line = outcomes_points) %>% 
  filter(market_name != "error") %>% 
  pivot_wider(id_cols = c(market_site, merge_name, market_name, market_line, alt_flag),
              values_from = market_odds_prob,
              names_from = over_under)

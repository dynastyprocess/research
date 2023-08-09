library(tidyverse)
library(rvest)
library(httr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0'
)

get_betmgm_page <- function(event_id = 13628605){
  
  Sys.sleep(2)
  res <- httr::GET(url = glue::glue("https://sports.il.betmgm.com/cds-api/bettingoffer/fixture-view?x-bwin-accessid=ZTg4YWEwMTgtZTlhYy00MWRkLWIzYWYtZjMzODI5ZDE0Mjc5&lang=en-us&country=US&userCountry=US&subdivision=US-Illinois&offerMapping=All&scoreboardMode=Full&fixtureIds={event_id}&state=Latest&includePrecreatedBetBuilder=true&supportVirtual=false&useRegionalisedConfiguration=true"),
                   httr::add_headers(.headers=headers))
  
  json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))
  
  return(json_obj[["fixture"]][["games"]])
  
}

res <- httr::GET(url = "https://sports.il.betmgm.com/en/api/clientconfig/partial?configNames=msMarqueeTiles&forceFresh=1",
                 httr::add_headers(.headers=headers))


json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))

betmgm_props <- 
  tibble(events = json_obj[["msMarqueeTiles"]][["tiles"]]) %>%
  unnest_wider(events) %>%
  unnest(eventIds) %>% 
  filter(sportId == 11, competitionId == 35, marketType == "WinningMargin") %>% 
  distinct(event_id = as.character(eventIds)) %>% 
  # select(event_id = key) %>%
  # tibble(event_id = 13628605) %>% 
  mutate(nested_markets = map(.x = event_id, .f = get_betmgm_page)) %>% 
  unnest(nested_markets)

betmgm_props_wide <- 
  betmgm_props %>% 
  unnest_wider(nested_markets) %>% 
  unnest(results) %>% 
  unnest_wider(results, names_sep = "_") %>%
  unnest_wider(name, names_sep = "_") %>% 
  unnest_wider(results_name, names_sep = "_") %>% 
  unnest_wider(player1) %>% 
  
  transmute(
    market_site = "betmgm",

    # name_value,
    # results_name_value,
    # results_attr,
    
    market_name = case_when(str_detect(name_value, "rushing and receiving yards") ~ "rushing_receiving_yards",
                            
                            str_detect(name_value, "many receptions") ~ "receptions",
                            str_detect(results_name_value, "Receptions") ~ "receptions",
                            
                            str_detect(name_value, "many receiving yards ") ~ "receiving_yards",
                            str_detect(results_name_value, "Receiving Yards") ~ "receiving_yards",
                            
                            str_detect(name_value, "many rushing yards") ~ "rushing_yards",
                            str_detect(results_name_value, "Rushing Yards") ~ "rushing_yards",
                            
                            str_detect(name_value, "many rushing attempts") ~ "rushing_attempts",
                            str_detect(results_name_value, "Rushing Attempts") ~ "rushing_attempts",
                            
                            str_detect(name_value, "many passing yards") ~ "passing_yards",
                            str_detect(results_name_value, "Passing Yards") ~ "passing_yards",
                            
                            str_detect(name_value, "many interceptions") ~ "passing_interceptions",
                            str_detect(results_name_value, "Interceptions") ~ "passing_interceptions",
                            
                            str_detect(name_value, "many passing attempts") ~ "passing_attempts",
                            str_detect(name_value, "many pass completions") ~ "passing_completions",
                            str_detect(name_value, "many passing touchdowns") ~ "passing_touchdowns",
                            
                            str_detect(name_value, "player will score a touchdown") ~ "rushing_receiving_touchdowns",
                            str_detect(name_value, "player will score 2 or more touchdowns") ~ "rushing_receiving_touchdowns",
                            
                            TRUE ~ "error"),
    
    name_selector = if_else(market_name == "rushing_receiving_touchdowns",
                            results_name_value,
                            short),
    merge_name = nflreadr::clean_player_names(name_selector),
    
    alt_flag = str_detect(results_name_value, "To Record") | str_detect(results_name_value, "To Throw"),
    over_under = case_when(alt_flag ~ "over",
                           market_name == "rushing_receiving_touchdowns" ~ "over",
                           str_detect(results_name_value, "Over") ~ "over",
                           str_detect(results_name_value, "Under") ~ "under",
                           TRUE ~ "error"),
    market_odds_prob = odds.converter::odds.us2prob(results_americanOdds),
    market_line = case_when(str_detect(name_value, "player will score a touchdown") ~ 0.5,
                            str_detect(name_value, "player will score 2 or more touchdowns") ~ 1.5,
                            is.na(results_attr) ~ readr::parse_number(results_name_value) - 0.5,
                            TRUE ~ as.numeric(results_attr))) %>% 
  filter(market_name != "error",
         merge_name != "No Touchdown") %>% 
  pivot_wider(id_cols = c(market_site, merge_name, market_name, market_line, alt_flag),
              values_from = market_odds_prob,
              names_from = over_under,
              values_fn = min
              )


# betmgm_props_wide %>%
#   dplyr::group_by(market_site, merge_name, market_name, market_line, alt_flag, over_under) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)
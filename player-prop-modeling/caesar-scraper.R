require(httr)
library(tidyverse)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0'
)


get_game_markets <- function(id){
  
  Sys.sleep(2)
  res <- httr::GET(url = glue::glue('https://www.williamhill.com/us/il/bet/api/v3/events/{id}'),
                   httr::add_headers(.headers=headers))
  
  
  json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))
  
  return(json_obj[["markets"]])
}

find_event_ids <- httr::GET(url = "https://www.williamhill.com/us/il/bet/api/v3/events/highlights/?competitionId=007d7c61-07a7-4e18-bb40-15104b6eac92")

json_obj <- jsonlite::parse_json(content(find_event_ids, type = "text", encoding = "UTF-8"),
                                 httr::add_headers(.headers=headers))

caesar_props <- 
  tibble(markets = json_obj) %>% 
  unnest_wider(markets) %>% 
  unnest_longer(competitions) %>% 
  unnest_wider(competitions, names_sep = "_") %>% 
  filter(competitions_name == "NFL") %>% 
  unnest_longer(competitions_events) %>% 
  hoist(competitions_events, "id") %>% 
  select(game_id = id) %>% 
  mutate(nested_markets = map(.x = game_id, .f = get_game_markets))

caesar_props_wide <- 
  caesar_props %>% 
  unnest(nested_markets) %>% 
  unnest_wider(nested_markets) %>% 
  unnest_longer(selections) %>% 
  unnest_wider(selections, names_sep = "_") %>% 
  unnest_wider(selections_price, names_sep = "_") %>%
  separate(col = name, into = c("name","market"), sep = "\\|\\s\\|") %>% 
  filter(display) %>% 
  transmute(
    market_site = "casesars",
    market_name_clean = str_remove_all(displayName, "\\|"),
    name_selector = if_else(market_name_clean %in% c("Anytime Touchdown Scorer",
                                                     "Player To Score 2 or More Touchdowns"),
                            selections_name,
                            name),
    merge_name = nflreadr::clean_player_names(str_trim(str_remove_all(name_selector, "\\|"))),
    market_name = case_when(market_name_clean == "Player Total Rushing Attempts" ~ "rushing_attempts",
                            market_name_clean == "Player Total Rushing Yards" ~ "rushing_yards",
                            market_name_clean == "Player Total Rushing + Receiving Yards" ~ "rushing_receiving_yards",
                            market_name_clean == "Total Receptions" ~ "receptions",
                            market_name_clean == "Player Total Receiving Yards" ~ "receiving_yards",
                            market_name_clean == "Player Total Passing Touchdowns" ~ "passing_touchdowns",
                            market_name_clean == "Player Total Interceptions" ~ "passing_interceptions",
                            market_name_clean == "Player Total Passing Yards" ~ "passing_yards",
                            market_name_clean == "Player Total Passing Completions" ~ "passing_completions",
                            market_name_clean == "Player Total Passing Attempts" ~ "passing_attempts",
                            market_name_clean == "Anytime Touchdown Scorer" ~ "rushing_receiving_touchdowns",
                            market_name_clean == "Player To Score 2 or More Touchdowns" ~ "rushing_receiving_touchdowns",
                            TRUE ~ "error"),
    alt_flag = FALSE,
    over_under = case_when(market_name_clean == "Anytime Touchdown Scorer" ~ "over",
                           market_name_clean == "Player To Score 2 or More Touchdowns" ~ "over",
                           TRUE ~ selections_type),
    market_odds_prob = odds.converter::odds.dec2prob(selections_price_d),
    market_line = case_when(market_name_clean == "Anytime Touchdown Scorer" ~ 0.5,
                            market_name_clean == "Player To Score 2 or More Touchdowns" ~ 1.5,
                            TRUE ~ line)) %>% 
  filter(market_name != "error", !is.na(market_odds_prob)) %>% 
  pivot_wider(id_cols = c(market_site, merge_name, market_name, market_line, alt_flag),
              values_from = market_odds_prob,
              names_from = over_under)


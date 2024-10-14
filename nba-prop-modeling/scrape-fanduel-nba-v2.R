library(tidyverse)
library(rvest)
library(httr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0'
)

get_fanduel_page <- function(page, event_id){
  
  Sys.sleep(4)
  res <- httr::GET(url = glue::glue('https://sbapi.il.sportsbook.fanduel.com/api/event-page?betexRegion=GBR&capiJurisdiction=intl&currencyCode=USD&exchangeLocale=en_US&includePrices=true&language=en&priceHistory=1&regionCode=NAMERICA&_ak=FhMFpcPWXMeyZxOx&eventId={event_id}&tab={page}'),
                   httr::add_headers(.headers=headers))
  
  json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))
  
  return(json_obj[["attachments"]])
  
}

# Get all the event IDs for the coming week
res <- httr::GET(url = "https://sbapi.il.sportsbook.fanduel.com/api/content-managed-page?betexRegion=GBR&capiJurisdiction=intl&currencyCode=USD&exchangeLocale=en_US&includePrices=true&includeRaceCards=false&includeSeo=true&language=en&regionCode=NAMERICA&timezone=America/Chicago&includeMarketBlurbs=true&page=CUSTOM&customPageId=nba&_ak=FhMFpcPWXMeyZxOx",
                 httr::add_headers(.headers=headers))

json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))

 
fanduel_props <- 
  tibble(games = json_obj[["attachments"]][["events"]]) %>% 
  unnest_wider(games) %>% 
  filter(str_detect(name, "\\@")) %>%
  head(3) |> 
  # filter(openDate <= lubridate::today(tzone = "EST") + days(7)) |> 
  distinct(event_id = eventId) %>% 
  crossing(prop_tabs = c("player-points", "player-rebounds", "player-assists", "player-threes", "player-combos")) %>% 
  mutate(nested_markets = map2(.x = prop_tabs, 
                               .y = event_id, 
                               .f = possibly(.f = get_fanduel_page,
                                              otherwise = NULL)))

fanduel_props_wide <- 
  fanduel_props %>% 
  unnest(nested_markets) %>% 
  unnest_longer(nested_markets) %>% 
  unnest_wider(nested_markets) %>% 
  unnest_longer(runners) %>% 
  unnest_wider(runners, names_sep = "_") %>% 
  hoist(runners_winRunnerOdds, "trueOdds") %>% 
  hoist(trueOdds, "decimalOdds") %>% 
  unnest(decimalOdds) %>% 
  hoist(runners_result, "type") %>% 
  separate(marketName, into = c("name","market_name_raw"), sep = "\\s\\-\\s") %>%
  
  transmute(
    market_site = "fanduel",
    merge_name = if_else(prop_tabs == "td-scorer-props", runners_runnerName, name),
    merge_name = nflreadr::clean_player_names(merge_name),
    market_name = case_when(market_name_raw == "Rushing + Receiving Yds" ~ "rushing_receiving_yards",
                            market_name_raw %in% c("Alt Receiving Yds", "Receiving Yds") ~ "receiving_yards",
                            market_name_raw %in% c("Alt Rushing Yds", "Rushing Yds") ~ "rushing_yards",
                            market_name_raw == "Total Receptions" ~ "receptions",
                            market_name_raw == "Rush Attempts" ~ "rushing_attempts",
                            market_name_raw == "Pass Attempts" ~ "passing_attempts",
                            market_name_raw == "Pass Completions" ~ "passing_completions",
                            market_name_raw == "Interceptions" ~ "passing_interceptions",
                            market_name_raw %in% c("Alt Passing Yds","Passing Yds") ~ "passing_yards",
                            market_name_raw == "Passing TDs" ~ "passing_touchdowns",
                            name == "Any Time Touchdown Scorer" ~ "rushing_receiving_touchdowns",
                            name == "To Score 2+ Touchdowns" ~ "rushing_receiving_touchdowns",
                            TRUE ~ "error"),
    alt_flag = if_else(prop_tabs == "td-scorer-props", FALSE, str_detect(market_name_raw, "Alt")),
    over_under = case_when(str_detect(market_name_raw, "Alt") ~ "over",
                           prop_tabs == "td-scorer-props" ~ "over",
                           TRUE ~ str_to_lower(type)),
    market_odds_prob = odds.converter::odds.dec2prob(as.numeric(decimalOdds)),
    
    market_line = case_when(
      name == "Any Time Touchdown Scorer" ~ 0.5,
      name == "To Score 2+ Touchdowns" ~ 1.5,
      runners_handicap == 0 ~ readr::parse_number(str_remove_all(runners_runnerName, "\\.|\\-")) - 0.5,
      TRUE ~ runners_handicap)) %>% 
  filter(market_name != "error") %>% 
  pivot_wider(id_cols = c(market_site, merge_name, market_name, market_line, alt_flag),
              values_from = market_odds_prob,
              names_from = over_under)

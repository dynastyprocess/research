# The DraftKings scraper hits state specific API that is controlled using
# category and subcategory. The combination of category and subcategory
# can be found by watching the "Network" tab on the inspector or by observing
# the returned JSON object before any filtering.
# https://sportsbook.draftkings.com/leagues/football/nfl?category=rush%2Frec-props&subcategory=rush-yds

library(tidyverse)
library(rvest)
library(httr)

headers = c(`User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0')

get_draftkings_sgp <- function(event_id = NULL){
  res <- httr::GET(url = glue::glue('https://sportsbook-us-il.draftkings.com//sites/US-IL-SB/api/v3/event/26843988?format=json'))
  
  json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"),
                                   httr::add_headers(.headers=headers))
  
  return(json_obj[["eventCategories"]])
  
}

draftkings_sgp_props <- 
  tibble(event_id = "fake") %>% 
  mutate(nested_market = map(.x = event_id, .f = get_draftkings_sgp)) 

draftkings_sgp_props_wide <- 
  draftkings_sgp_props %>% 
  unnest(nested_market) %>% 
  unnest_wider(nested_market) %>% 
  filter(name == "Rush/Rec Props") %>%
  # filter(offerSubcategoryDescriptors != "NULL") %>% 
  unnest_longer(componentizedOffers) %>% 
  # hoist(.col = "offerSubcategoryDescriptors", "offerSubcategory") %>% 
  hoist(.col = "componentizedOffers", "offers") %>% 
  unnest_longer(offers) %>% 
  unnest_longer(offers) %>% 
  unnest_wider(offers) %>% 
  unnest(outcomes) %>% 
  unnest_wider(outcomes, names_sep = "_") %>% 
  filter(isOpen) %>% 
  transmute(market_site = "draftkings",
            merge_name = if_else(market_name == "rushing_receiving_touchdowns", outcomes_label, outcomes_participant),
            merge_name = nflreadr::clean_player_names(merge_name),
            market_name,
            alt_flag = str_detect(outcomes_label, "\\+"),
            over_under = case_when(alt_flag ~ "over",
                                   market_name == "rushing_receiving_touchdowns" ~ "over",
                                   TRUE ~ str_to_lower(outcomes_label)),
            market_odds_prob = odds.converter::odds.dec2prob(outcomes_oddsDecimal),
            market_line = case_when(alt_flag ~ readr::parse_number(outcomes_label) - 0.5,
                                    label == "Anytime Touchdown Scorer" ~ 0.5,
                                    label == "Score 2 or More Touchdowns" ~ 1.5,
                                    TRUE ~ outcomes_line)) %>% 
  pivot_wider(id_cols = c(market_site, merge_name, market_name, market_line, alt_flag),
              values_from = market_odds_prob,
              names_from = over_under)






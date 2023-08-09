# The DraftKings scraper hits state specific API that is controlled using
# category and subcategory. The combination of category and subcategory
# can be found by watching the "Network" tab on the inspector or by observing
# the returned JSON object before any filtering.
# https://sportsbook.draftkings.com/leagues/football/nfl?category=rush%2Frec-props&subcategory=rush-yds

library(tidyverse)
library(rvest)
library(httr)

headers = c(`User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0')

get_draftkings_subcategory <- function(category, subcategory){
  res <- httr::GET(url = glue::glue('https://sportsbook-us-il.draftkings.com//sites/US-IL-SB/api/v5/eventgroups/88808/categories/{category}/subcategories/{subcategory}?format=json'))
  
  json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"),
                                   httr::add_headers(.headers=headers))
  
  return(json_obj[["eventGroup"]][["offerCategories"]])
  
}

draftkings_props <- 
  tibble(category_number = c(1000,
                             1000,
                             1000,
                             1000,
                             1000,
                             1000,
                             
                             1001,
                             1001,
                             1001,
                             1001,
                             1001,
                             1001,
                             1001,
                             1001,
                             
                             1003,
                             1003),
         subcategory_number = c(9517,
                                9522,
                                9524,
                                12093,
                                9525,
                                9516,
                                
                                9514,
                                9512,
                                9523,
                                9519,
                                9518,
                                12094,
                                12095,
                                12096,
                                
                                11819,
                                12421),
         market_name = c("passing_attempts",
                         "passing_completions",
                         "passing_yards",
                         "passing_yards",
                         "passing_touchdowns",
                         "passing_interceptions",
                         
                         "rushing_yards",
                         "receiving_yards",
                         "rushing_receiving_yards",
                         "receptions", 
                         "rushing_attempts",
                         "rushing_yards",
                         "receiving_yards",
                         "rushing_receiving_yards",
                         
                         "rushing_receiving_touchdowns",
                         "rushing_receiving_touchdowns")) %>% 
  mutate(nested_market = map2(.x = category_number, .y = subcategory_number, .f = get_draftkings_subcategory)) 

draftkings_props_wide <- 
  draftkings_props %>% 
  unnest(nested_market) %>% 
  unnest_wider(nested_market) %>% 
  # filter(name == "Rush/Rec Props") %>% 
  filter(offerSubcategoryDescriptors != "NULL") %>% 
  unnest_longer(offerSubcategoryDescriptors) %>% 
  hoist(.col = "offerSubcategoryDescriptors", "offerSubcategory") %>% 
  hoist(.col = "offerSubcategory", "offers") %>% 
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






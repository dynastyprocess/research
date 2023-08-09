library(tidyverse)
library(rvest)
library(httr)

remove_vig_power <- function(away_prob, home_prob, overround_limit = 1e-5, verbose = FALSE){
  probs <- c(away_prob, home_prob)
  # see http://dx.doi.org/10.11648/j.ajss.20170506.12
  n <- length(probs)
  pi <- sum(probs) # booksum
  error <- abs(pi-1) # overround
  # to check how many iterations were necessary
  if(isTRUE(verbose)) cli::cli_alert_info("overround: {.val {error}}")
  if(error <= overround_limit) return(probs)
  k <- log(n) / log(n / pi)
  new_probs <- probs ^ k
  remove_vig_power(new_probs[1], new_probs[2], overround_limit = overround_limit, verbose = verbose)
}

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0'
)

get_fanduel_page <- function(page, event_id){
  
  Sys.sleep(2)
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
  distinct(event_id = eventId) %>% 
  # filter(event_id == 31927586) %>% 
  crossing(prop_tabs = c("player-points", "player-rebounds", "player-assists", "player-threes", "player-combos")) %>% 
  mutate(nested_markets = map2(.x = prop_tabs, .y = event_id, .f = get_fanduel_page))

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
    merge_name = nflreadr::clean_player_names(name),
    market_name = case_when(market_name_raw == "Assists" ~ "assists",
                            market_name_raw == "MISSING" ~ "blks_stls",
                            market_name_raw == "MISSING" ~ "blocks",
                            market_name_raw == "MISSING" ~ "double_doubles",
                            market_name_raw == "MISSING" ~ "fantasy_points",
                            market_name_raw == "MISSING" ~ "free_throws_made",
                            market_name_raw == "Points" ~ "points",
                            market_name_raw == "Pts + Ast" ~ "pts_asts",
                            market_name_raw == "Pts + Reb" ~ "pts_rebs",
                            market_name_raw == "Pts + Reb + Ast" ~ "pts_rebs_asts",
                            market_name_raw == "Rebounds" ~ "rebounds",
                            market_name_raw == "Reb + Ast" ~ "rebs_asts",
                            market_name_raw == "MISSING" ~ "steals",
                            market_name_raw == "Made Threes" ~ "three_points_made",
                            market_name_raw == "MISSING" ~ "turnovers",

                            TRUE ~ "error"),
    alt_flag = FALSE,
    over_under = case_when(str_detect(market_name_raw, "Alt") ~ "over",
                           prop_tabs == "td-scorer-props" ~ "over",
                           TRUE ~ str_to_lower(type)),
    market_odds_prob = odds.converter::odds.dec2prob(as.numeric(decimalOdds)),
    market_line = runners_handicap) %>% 
  filter(market_name != "error") %>% 
  distinct() %>% 
  pivot_wider(id_cols = c(market_site, merge_name, market_name, market_line, alt_flag),
              values_from = market_odds_prob,
              names_from = over_under) %>%
  mutate(unvigged_odds = map2(over, under, remove_vig_power)) %>%
  unnest_wider(unvigged_odds, names_sep = "_") %>%
  mutate(over = unvigged_odds_1, under = unvigged_odds_2) %>% 
  select(-contains("unvigged"))

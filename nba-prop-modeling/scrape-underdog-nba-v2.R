library(tidyverse)
require(httr)

library(tidyverse)
library(httr)
library(nflreadr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0'
)

res <- httr::GET(url = 'https://api.underdogfantasy.com/beta/v5/over_under_lines',
                 httr::add_headers(.headers=headers))
json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))


underdog_props_wide <- 
  tibble(ou_lines = json_obj[["over_under_lines"]]) %>% 
  unnest_wider(col = ou_lines) %>% 
  unnest_wider(col = over_under, names_sep = ".") %>% 
  unnest_wider(col = "over_under.appearance_stat", names_sep = ".")

underdog_props_wide <- 
  tibble(ou_lines = json_obj[["over_under_lines"]]) %>% 
  unnest_wider(col = ou_lines) %>% 
  unnest_wider(col = over_under, names_sep = ".") %>% 
  unnest_wider(col = "over_under.appearance_stat", names_sep = ".") %>% 
  transmute(
    market_site = "underdog",
    merge_name = nflreadr::clean_player_names(
      str_extract(over_under.title, "^(.+?)(?=(( Assists)|( Blocks)|( Double)|( Fantasy)|( FT)|( Points)|( Pts)|( Rebounds)|( Steals)|( 3-Pointers)|( Turnovers)))")),
    market_name = over_under.appearance_stat.stat,
    market_line = as.numeric(stat_value),
    alt_flag = FALSE,
    over = 0.5,
    under = 0.5) %>% 
  filter(market_name %in% c("assists"))


underdog_join <- 
  underdog_props_wide %>% 
  inner_join(assist_preds_long, by = c("market_name", 
                                       "market_line", 
                                       "merge_name" = "player_name")) %>% 
  
  bind_rows(
    underdog_props_wide %>% 
      inner_join(assist_preds_long, by = c("market_name",
                                       "market_line" = "pred_category",
                                       "merge_name" = "player_name")) |> 
      mutate(under_prob = under_prob - value)
    
  ) |> 
  
  mutate(under_ev = under_prob - under,
         over_ev = over_prob - over,
         best_ev = pmax(under_ev, over_ev, na.rm = TRUE),
         over_under = if_else(best_ev == over_ev, "over", "under"),
         expected_odds_prob = if_else(over_under == "over", over_prob, under_prob),
         market_odds_prob = if_else(over_under == "over", over, under),
         market_odds_frac = (1 / (market_odds_prob)) - 1,
         regress_to_market = expected_odds_prob,
         # regress_to_market = (expected_odds_prob + market_odds_prob) / 2,
         kelly_frac = regress_to_market-((1-regress_to_market)/market_odds_frac),
         kelly_bet = round(200*kelly_frac,2)) %>% 
  transmute(market_site,
            merge_name,

            market_name,
            market_class = case_when(market_name %in% c("rushing_yards", "rushing_attempts") ~ "rushing",
                                     market_name %in% c("receiving_yards", "receptions") ~ "receiving",
                                     market_name %in% c("passing_yards", "passing_attempts", "passing_completions", "passing_touchdowns", "passing_interceptions") ~ "passing",
                                     market_name %in% c("rushing_receiving_yards", "rushing_receiving_touchdowns", "rushing_receiving_yards", "anytime_touchdown") ~ "combo",
                                     TRUE ~ "error"),
            market_line,
            over_under,
            market_odds_prob,
            market_odds_us = round(odds.converter::odds.prob2us(market_odds_prob)),
            expected_odds_us = round(odds.converter::odds.prob2us(expected_odds_prob)),
            expected_odds_prob,
            regress_to_market,
            kelly_frac = round(kelly_frac, 4),
            best_ev = round(best_ev, 3),
            kelly_bet) |> 
  group_by(market_name, merge_name) |> 
  mutate(best_kelly = max(kelly_bet),
         best_site = if_else(best_kelly == kelly_bet, market_site, NA_character_)) |> 
  fill(best_site,
       .direction = "updown") |> 
  ungroup()

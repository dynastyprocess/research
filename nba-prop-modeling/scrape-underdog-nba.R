library(tidyverse)
require(httr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0'
)

res <- httr::GET(url = 'https://api.underdogfantasy.com/beta/v3/over_under_lines',
                 httr::add_headers(.headers=headers))
json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))


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
  filter(market_name %in% c("assists",
                            "blks_stls",
                            "blocks",
                            "double_doubles",
                            "fantasy_points",
                            "free_throws_made",
                            "points",
                            "pts_asts",
                            "pts_rebs",
                            "pts_rebs_asts",
                            "rebounds",
                            "rebs_asts",
                            "steals",
                            "three_points_made",
                            "turnovers"))

source("~/Documents/DynastyProcess/research/nba-prop-modeling/scrape-fanduel-nba.R")

combine_lines_preds <-
  underdog_props_wide %>%
  left_join(fanduel_props_wide, by = c("market_name","market_line", "merge_name")) %>% 
  mutate(over_ev = over.y - over.x,
         under_ev = under.y - under.x,
         best_ev = pmax(under_ev, over_ev, na.rm = TRUE),
         over_under = if_else(best_ev == over_ev, "over", "under"),
         across(where(is.numeric),
                ~round(.x, 4))) %>%
  select(-over.x, - under.x, -alt_flag.x, -alt_flag.y, -market_site.y)
  
  
  mutate(under_ev = under_prob - under,
         over_ev = over_prob - over,
         best_ev = pmax(under_ev, over_ev, na.rm = TRUE),
         over_under = if_else(best_ev == over_ev, "over", "under"),
         expected_odds_prob = if_else(over_under == "over", over_prob, under_prob),
         market_odds_prob = if_else(over_under == "over", over, under),
         market_odds_frac = (1 / (market_odds_prob)) - 1,
         kelly_frac = expected_odds_prob-((1-expected_odds_prob)/market_odds_frac))

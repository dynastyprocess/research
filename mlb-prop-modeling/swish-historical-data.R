library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(baseballr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0'
)

get_swish_batters <- function(date) {
  Sys.sleep(runif(1, 1, 6))
  print(date)
  
  x <- httr::POST(url = "https://swishanalytics.com/optimus/mlb/ajax/dfs-batter-projections-ajax.php",
                  body = list(date = date),
                  httr::add_headers(.headers=headers))
  
  y <- content(x, as = "text") %>% jsonlite::parse_json()
  
  return(y)
}

get_swish_pitchers <- function(date) {
  Sys.sleep(runif(1, 1, 6))
  print(date)
  
  x <- httr::POST(url = "https://swishanalytics.com/optimus/mlb/ajax/dfs-pitcher-projections-ajax.php",
                  body = list(date = date),
                  httr::add_headers(.headers=headers))
  
  y <- content(x, as = "text") %>% jsonlite::parse_json()
  
  return(y)
}

swish_pitchers <- 
  tibble(date_input = as_date("2022-04-07"):as_date("2022-11-05")) %>% 
  mutate(date_input = as.character(as_date(date_input))) %>% 
  # head(5) %>% 
  mutate(pitcher_obj = map(date_input, get_swish_pitchers))

saveRDS(swish_pitchers, "swish_pitchers_2022.RDS")

all_ids <- chadwick_player_lu()

pitcher_ids <-
  fg_pitcher_leaders(
    x = 2022,
    y = 2022,
    league = "all",
    qual = "n",
    pitcher_type = "pit",
    ind = 1
  )

pticher_ex <- 
  pitcher_ids %>% 
  distinct(playerid) %>%
  mutate(fg_obj = map2(.x = playerid,
                       .y = 2022, 
                       .f = fg_pitcher_game_logs, 
                       .progress = TRUE))

pitching_game_logs <- 
  pticher_ex %>% 
  select(fg_obj) %>% 
  unnest(fg_obj) 
  # group_by(playerid, Date) %>% 
  # slice_head(n = 1) %>% 
  # mutate(game_number = rev(row_number())) %>% 
  # ungroup()

swish_pitchers_unnest <- 
  swish_pitchers %>% 
  unnest(pitcher_obj) %>% 
  unnest_wider(pitcher_obj) %>%
  group_by(player_id, date_input) %>%
  slice_head(n = 1) %>% 
  # mutate(game_number = row_number()) %>% 
  ungroup() %>% 
  left_join(all_ids %>% 
              transmute(player_id = as.character(key_mlbam), key_fangraphs),
            by = c("player_id")) %>% 
  left_join(pitching_game_logs %>% 
              select(playerid, Date, SO),
            by = c("key_fangraphs" = "playerid",
                   "date_input" = "Date"))

strikeout_data <- 
  swish_pitchers_unnest %>% 
  mutate(predicted_so = as.numeric(so)) %>% 
  filter(predicted_so >= 2,
         !is.na(SO))

saveRDS(strikeout_data, "strikeout_data.RDS")


swish_batters <- 
  tibble(date_input = as_date("2022-04-07"):as_date("2022-11-05")) %>% 
  mutate(date_input = as.character(as_date(date_input))) %>% 
  head(5) %>% 
  mutate(batter_obj = map(date_input, get_swish_batters))

batter_ids <-
  fg_batter_leaders(
    x = 2022,
    y = 2022,
    league = "all",
    qual = "n", 
    exc_p = FALSE,
    ind = 1
  )

batter_ex <- 
  batter_ids %>% 
  distinct(playerid) %>%
  mutate(fg_obj = map2(.x = playerid,
                       .y = 2022, 
                       .f = fg_batter_game_logs, 
                       .progress = TRUE))

batting_game_logs <- 
  batter_ex %>% 
  select(fg_obj) %>% 
  unnest(fg_obj) %>% 
  group_by(playerid, Date) %>% 
  mutate(game_number = rev(row_number())) %>% 
  ungroup()

swish_batters_unnest <- 
  swish_batters %>% 
  unnest(batter_obj) %>% 
  unnest_wider(batter_obj) %>% 
  group_by(player_id, date_input) %>% 
  mutate(game_number = row_number(time)) %>% 
  ungroup() %>% 
  left_join(all_ids %>% 
              transmute(player_id = as.character(key_mlbam), key_fangraphs),
            by = c("player_id")) %>% 
  left_join(batting_game_logs %>% 
              select(playerid, Date, Team, SO, Opp, BatOrder, game_number),
            by = c("key_fangraphs" = "playerid",
                   "date_input" = "Date",
                   "game_number")) %>% 
  mutate(Opp = str_remove(Opp, "@"))

game_batters_pivot <- 
  swish_batters_unnest %>% 
  pivot_wider(id_cols = c(date_input, game_number, Team),
              names_from = BatOrder,
              names_glue = "{.value}_batter_{BatOrder}",
              values_from = c(outs, ab, bats))
              





# pitchers <- bref_daily_pitcher("2015-05-10", "2015-06-20")
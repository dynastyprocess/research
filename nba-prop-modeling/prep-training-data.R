library(hoopR)
library(tidyverse)


nba_team_box <- hoopR::load_nba_team_box(2023)

nba_player_box <- hoopR::load_nba_player_box(2023)


nba_team_sched <- load_nba_schedule(seasons = 2023)

nba_play_by_play <- load_nba_pbp(2023)
nba_pbp <- nba_play_by_play %>% filter(game_id == 401468016)
nba_pbp2 <- espn_nba_pbp(game_id = 401468016)

nba_betting_data <- espn_nba_betting(401442535)

nba_player_track <- nba_boxscoreplayertrackv2(401468615)


test <- nba_data_pbp(game_id = "0021900001")

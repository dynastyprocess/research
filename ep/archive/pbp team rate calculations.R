group_by(game_id, posteam) %>%
  mutate(team_air_yards = slide_dbl(air_yards, ~sum(.x, na.rm = TRUE), .before = Inf, .after = -1),
         team_rec_yards = slide_dbl(yards_gained, ~sum(.x, na.rm = TRUE), .before = Inf, .after = -1),
         team_tds = slide_dbl(pass_touchdown, ~sum(.x, na.rm = TRUE), .before = Inf, .after = -1),
         team_attempts = slide_dbl(pass_attempt, ~sum(.x, na.rm = TRUE), .before = Inf, .after = -1)) %>%
  ungroup() %>%
  
  group_by(game_id, posteam) %>%
  mutate(team_rush_yards = slide_dbl(yards_gained, ~sum(.x, na.rm = TRUE), .before = Inf, .after = -1),
         team_touchdowns = slide_dbl(rush_touchdown, ~sum(.x, na.rm = TRUE), .before = Inf, .after = -1),
         team_attempts = slide_dbl(rush_attempt, ~sum(.x, na.rm = TRUE), .before = Inf, .after = -1)) %>%
  ungroup() %>%

new_game_flag = ifelse(lag(game_id) == game_id, 0, 1),
new_game_flag = ifelse(is.na(new_game_flag),1,new_game_flag),

new_team_airyards = ifelse(new_game_flag == 1, team_air_yards, team_air_yards - lag(team_air_yards)), 
new_team_attempts = ifelse(new_game_flag == 1, team_attempts, team_attempts - lag(team_attempts)), 
new_team_yards = ifelse(new_game_flag == 1, team_rec_yards, team_rec_yards - lag(team_rec_yards)), 
new_team_tds = ifelse(new_game_flag == 1, team_tds, team_tds - lag(team_tds)), 

air_yards_share_ToDate = slide2_dbl(lag(air_yards), new_team_airyards, ~get_rate(.x,.y), .before = Inf, .after = 0),
air_yards_share_ToDate = ifelse(is.nan(air_yards_share_ToDate) | is.infinite(air_yards_share_ToDate),0,air_yards_share_ToDate),

yards_share_ToDate = slide2_dbl(lag(yards_gained), new_team_yards, ~get_rate(.x,.y), .before = Inf, .after = 0),
yards_share_ToDate = ifelse(is.nan(yards_share_ToDate) | is.infinite(yards_share_ToDate),0,yards_share_ToDate),

td_share_ToDate = slide2_dbl(lag(pass_touchdown), new_team_tds, ~get_rate(.x,.y), .before = Inf, .after = 0),
td_share_ToDate = ifelse(is.nan(td_share_ToDate) | is.infinite(td_share_ToDate),0,td_share_ToDate),


target_share_ToDate = slide2_dbl(lag(pass_attempt), new_team_attempts, ~get_rate(.x,.y), .before = Inf, .after = 0),
target_share_ToDate = ifelse(is.nan(target_share_ToDate) | is.infinite(target_share_ToDate),0,target_share_ToDate),


new_game_flag = ifelse(lag(game_id) == game_id, 0, 1),
new_game_flag = ifelse(is.na(new_game_flag),1,new_game_flag),

new_team_yards = ifelse(new_game_flag == 1, team_rush_yards, team_rush_yards - lag(team_rush_yards)), 
new_team_attempts = ifelse(new_game_flag == 1, team_attempts, team_attempts - lag(team_attempts)),
new_team_tds = ifelse(new_game_flag == 1, team_touchdowns, team_touchdowns - lag(team_touchdowns)),

rush_yards_share_ToDate = slide2_dbl(lag(yards_gained), new_team_yards, ~get_rate(.x,.y), .before = Inf, .after = 0),
rush_yards_share_ToDate = case_when(is.nan(rush_yards_share_ToDate) | is.infinite(rush_yards_share_ToDate) | rush_yards_share_ToDate < 0 ~ 0,
                                    rush_yards_share_ToDate > 1 ~ 1,
                                    TRUE ~ rush_yards_share_ToDate),

rush_td_share_ToDate = slide2_dbl(lag(rush_touchdown), new_team_tds, ~get_rate(.x,.y), .before = Inf, .after = 0),
rush_td_share_ToDate = case_when(is.nan(rush_td_share_ToDate) | is.infinite(rush_td_share_ToDate) | rush_td_share_ToDate < 0 ~ 0,
                                 rush_td_share_ToDate > 1 ~ 1,
                                 TRUE ~ rush_td_share_ToDate),

yards_per_team_attempt = slide2_dbl(lag(yards_gained), new_team_attempts, ~get_rate(.x,.y), .before = Inf, .after = 0),
yards_per_team_attempt = case_when(is.nan(yards_per_team_attempt) | is.infinite(yards_per_team_attempt) ~ 0,
                                   TRUE ~ yards_per_team_attempt),

attempt_share_ToDate = slide2_dbl(lag(rush_attempt), new_team_attempts, ~get_rate(.x,.y), .before = Inf, .after = 0),
attempt_share_ToDate = ifelse(is.na(attempt_share_ToDate) | is.infinite(attempt_share_ToDate),0,attempt_share_ToDate),

rush_share_attempt_share_diff = rush_yards_share_ToDate - attempt_share_ToDate,
library(tidyverse)
library(nflfastR)
library(arrow)
library(lubridate)
library(tidymodels)
library(DALEXtra)
library(DALEX)
library(iBreakDown)

model_data <- 
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/nflfastr_pbp") %>% 
  filter(season == 2020) %>% 
  dplyr::collect() %>%
  make_model_mutations() %>%
  filter(
    season >= 2006, complete_pass == 1, !is.na(yards_after_catch),
    yards_after_catch >= -20, air_yards < yardline_100
  ) %>%
  dplyr::mutate(
    distance_to_goal = yardline_100 - air_yards,
    pass_middle = dplyr::if_else(pass_location == "middle", 1, 0),
    air_is_zero = dplyr::if_else(air_yards == 0, 1, 0),
    distance_to_sticks = air_yards - ydstogo,
    yards_after_catch = dplyr::case_when(
      yards_after_catch < -5 ~ -5,
      yards_after_catch > 70 ~ 70,
      TRUE ~ yards_after_catch
    ),
    label = yards_after_catch + 5
  ) %>%
  dplyr::filter(!is.na(air_yards) & air_yards >= -15 & air_yards < 70 & !is.na(pass_location)) %>%
  select("air_yards","yardline_100" ,      "ydstogo"    ,       
         "distance_to_goal"  , "down1"    ,          "down2"         ,    
         "down3"      ,        "down4"        ,      "air_is_zero"     ,  
          "pass_middle"   ,     "era2"         ,      "era3"            ,  
          "era4"     ,          "qb_hit"     ,        "home"    ,          
          "outdoors"     ,      "retractable"    ,    "dome"     ,         
          "distance_to_sticks")
  # dplyr::select(
  #   season, label, air_yards, yardline_100, ydstogo, distance_to_goal,
  #   down1, down2, down3, down4, air_is_zero, pass_middle,
  #   era2, era3, era4, qb_hit, home,
  #   outdoors, retractable, dome, distance_to_sticks
  # )


temp_preds <- predict(temp, as.matrix(model_data)) %>% matrix(ncol = 76, byrow = TRUE) %>% as.

preds <- as.data.frame(
  matrix(predict(temp, as.matrix(model_data)), ncol = 76, byrow = TRUE)
)


bind_temp <- bind_cols(model_data, preds)


# Comparing Comp % models
get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}

start_year <- 2020

nflfastr_rosters <-
  nflfastR::fast_scraper_roster(start_year:2020) %>%
  select(season, gsis_id, position, full_name, birth_date, sportradar_id) %>% 
  mutate(position = dplyr::if_else(position %in% c("HB","FB"), "RB", position)) %>% 
  filter(!is.na(gsis_id))

pass_df <-
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/nflfastr_pbp") %>% 
  filter(season >= start_year) %>% 
  dplyr::collect() %>%
  filter(play_type == "pass",
         !str_detect(desc, "Aborted")) %>%
  left_join(select(nflfastr_rosters, gsis_id, season, passer_position = position, passer_birth_date = birth_date),
            by = c("passer_player_id" = "gsis_id", "season"),
            na_matches = "never") %>%
  left_join(select(nflfastr_rosters, gsis_id, season, receiver_position = position, receiver_birth_date = birth_date),
            by = c("receiver_player_id" = "gsis_id", "season"),
            na_matches = "never") %>%
  filter(passer_position %in% c("QB","RB","WR","TE")) %>% 
  filter(receiver_position %in% c("QB","RB","WR","TE")) %>%
  mutate(game_month = month(game_date),
         game_month = if_else(game_month < 3, 12, game_month),
         game_week = week(game_date),
         game_week = if_else(game_week <= 30, 53, game_week),
         game_wday = as.character(wday(game_date, label = TRUE)),
         game_wday = case_when(game_wday %in% c("Tue","Wed","Fri","Sat") ~ "Other",
                               TRUE ~ game_wday),
         
         game_time = hour(hms(start_time)),
         drive_play_count = if_else(is.na(drive_play_count), 7, drive_play_count),
         implied_total = case_when(posteam_type == "away" & spread_line<=0 ~ (total_line+spread_line)/2 - spread_line,
                                   posteam_type == "away" & spread_line>0 ~ (total_line-spread_line)/2,
                                   posteam_type == "home" & spread_line>0 ~ (total_line+spread_line)/2 - spread_line,
                                   posteam_type == "home" & spread_line<=0 ~ (total_line-spread_line)/2),
         
         passer_age = get_age(passer_birth_date, game_date, dec = TRUE),
         receiver_age = get_age(receiver_birth_date, game_date, dec = TRUE),
         passer_position = if_else(passer_position != "QB", "non-QB", passer_position),
         
         # contested_target = if_else(is.na(pass_defense_1_player_id), 0, 1),
         
         #Two Point Conversion fixes
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & str_detect(desc, "ATTEMPT SUCCEEDS") ~ 1,
                                         TRUE ~ 0),
         score = if_else(rush_touchdown == 1 | two_point_converted == 1, 1, 0),
         receiving_yards = case_when(is.na(receiving_yards) & two_point_attempt == 1 & two_point_converted == 1 ~ yardline_100,
                                     is.na(receiving_yards) & two_point_attempt == 1 & two_point_converted == 0 ~ 0,
                                     complete_pass == 0 ~ 0,
                                     TRUE ~ receiving_yards),
         air_yards = if_else(two_point_attempt == 1, yardline_100, air_yards),
         complete_pass = if_else(two_point_attempt == 1 & grepl("is complete", desc), 1, complete_pass),
         pass_complete = if_else(complete_pass == 1, "complete", "incomplete"),
         
         down = if_else(two_point_attempt == 1, 4, down),
         xpass = if_else(two_point_attempt == 1, 0.75, xpass),
         distance_to_sticks = air_yards - ydstogo,
         
         #Data Cleaning
         surface = if_else(surface == "grass", "grass", "turf"),
         pass_location = case_when(!is.na(pass_location) ~ pass_location,
                                   str_detect(desc, " left") ~ "left",
                                   str_detect(desc, " right") ~ "right",
                                   str_detect(desc, " middle") ~ "middle",
                                   TRUE ~ "unk"),
         temp = case_when(roof %in% c("closed", "dome") ~ 68L,
                          is.na(temp) ~ 60L,
                          TRUE ~ temp),
         wind = case_when(roof %in% c("closed", "dome") ~ 0L,
                          is.na(wind) ~ 8L,
                          TRUE ~ wind),
         zero_air_yards = if_else(air_yards == 0, 1, 0),
         
         receiving_fantasy_points = 6*pass_touchdown + 2*two_point_converted  + 0.1*receiving_yards - 2*fumble_lost + complete_pass,
         passing_fantasy_points =  4*pass_touchdown + 2*two_point_converted  + 0.04*receiving_yards - 2*fumble_lost - 2*interception,
         
         season = factor(season, levels = as.character(c(2001:2020)), ordered = TRUE),
         week = factor(week, levels = as.character(c(1:21)), ordered = TRUE),
         game_month = factor(game_month, levels = as.character(c(9:12)), ordered = TRUE),
         game_week = factor(game_week, levels = as.character(c(36:53)), ordered = TRUE),
         game_time = factor(game_time, levels = as.character(c(9:23)), ordered = TRUE),
         qtr = factor(qtr, levels = as.character(c(1:6)), ordered = TRUE),
         down = factor(down, levels = as.character(c(1:4)), ordered = TRUE),
         
         goal_to_go = factor(goal_to_go, levels = as.character(c(0,1))),
         shotgun = factor(shotgun, levels = as.character(c(0,1))),
         no_huddle = factor(no_huddle, levels = as.character(c(0,1))),
         qb_dropback = factor(qb_dropback, levels = as.character(c(0,1))),
         qb_scramble = factor(qb_scramble, levels = as.character(c(0,1))),
         two_point_attempt = factor(two_point_attempt, levels = as.character(c(0,1))),
         score = factor(score, levels = as.character(c(0,1))),
         first_down = factor(first_down, levels = as.character(c(0,1))),
         pass_complete = factor(pass_complete, levels = c("complete", "incomplete"), ordered = TRUE),
         qb_hit = factor(qb_hit, levels = as.character(c(0,1))),
         zero_air_yards = factor(zero_air_yards, levels = as.character(c(0,1)))
         # contested_target = factor(contested_target, levels = as.character(c(0,1)))
         
  )


fit_pass_completion <- readRDS("~/Documents/DynastyProcess/research/expected-points/models/fit_pass_completion.RDS")

pass_df_temp <- 
  pass_df %>% 
  bind_cols(predict(fit_pass_completion, pass_df, type = "prob")) %>% 
  rename(pass_completion_exp = .pred_1) %>% 
  mutate(cp_diff = cp - pass_completion_exp) %>% 
  select(passer_player_name, receiver_player_name, air_yards, cp, pass_completion_exp, .pred_0, cp_diff, pass_complete, complete_pass, play_id, game_id)
  
mn_log_loss_vec(pass_df_temp$pass_complete, pass_df_temp$pass_completion_exp)
mn_log_loss_vec(pass_df_temp$pass_complete, pass_df_temp$cp)

mn_log_loss_vec(as.factor(if_else(pass_df_temp$pass_complete == 1, 0, 1)), pass_df_temp$pass_completion_exp)
mn_log_loss_vec(as.factor(if_else(pass_df_temp$pass_complete == 1, 0, 1)), pass_df_temp$cp)

mn_log_loss_vec(as.factor(pass_df_temp$complete_pass), pass_df_temp$pass_completion_exp)
mn_log_loss_vec(as.factor(pass_df_temp$complete_pass), pass_df_temp$cp)


pass_completion_explainer <-
  explain_tidymodels(
    fit_pass_completion,
    data = select(pass_df, -complete_pass),
    y =  mutate(pass_df, complete_pass = as.numeric(complete_pass)) %>% pull(complete_pass))

pdp_time <- 
  model_profile(
    pass_completion_explainer,
    variables = "air_yards",
    groups = "down"
  )

pred_demo <- break_down(pass_completion_explainer,
                        pass_df %>% filter(play_id == 1273, game_id == "2020_03_TB_DEN"))



metric_ex <- 
  tibble(prob = runif(100),
         outcome = as.factor(round(runif(100))),
         outcome_flipped = as.factor(ifelse(outcome == 0, 1, 0)))

mn_log_loss_vec(metric_ex$outcome, metric_ex$prob)
mn_log_loss_vec(metric_ex$outcome_flipped, metric_ex$prob)







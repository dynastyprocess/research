library(DALEXtra)
library(iBreakDown)

#Libraries
library(tidyverse)
library(tidymodels)
library(here)
library(arrow)
library(nflfastR)
library(stacks)
library(lubridate)
library(baguette)
library(rules)
library(doParallel)

library(finetune)



## Functions

get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}



## Load the Data


rolling_df <- 
  arrow::open_dataset("~/Documents/DynastyProcess/research/expected-points/data/rolling_df") %>% 
  dplyr::collect()

rushing_df <-
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/nflfastr_pbp") %>% 
  filter(season >= 2007) %>% 
  dplyr::collect() %>%
  filter(play_type == "run",
         !str_detect(desc, "kneel|Aborted")) %>% 
  inner_join(rolling_df, by = c("fantasy_player_id"="gsis_id", "season", "week")) %>%
  filter(position %in% c("QB","RB","WR","TE")) %>%
  mutate(game_month = month(game_date),
         game_month = if_else(game_month < 3, 12, game_month),
         game_week = week(game_date),
         game_week = if_else(game_week <= 30, 53, game_week),
         game_wday = as.character(wday(game_date, label = TRUE)),
         game_wday = case_when(game_wday %in% c("Tue","Wed","Fri","Sat") ~ "Other",
                               TRUE ~ game_wday),
         
         rusher_age = get_age(birth_date, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & str_detect(desc, "ATTEMPT SUCCEEDS") ~ 1,
                                         TRUE ~ 0),
         score = if_else(rush_touchdown == 1 | two_point_converted == 1, 1, 0),
         yards_gained = if_else(two_point_attempt == 1 & two_point_converted == 1, yardline_100, yards_gained),
         # yards_gained = log10(yards_gained),
         
         down = if_else(two_point_attempt == 1, 4, down),
         
         surface = if_else(surface == "grass", "grass", "turf"),
         
         run_location = case_when(!is.na(run_location) ~ run_location,
                                  str_detect(desc, " left") ~ "left",
                                  str_detect(desc, " right") ~ "right",
                                  str_detect(desc, " middle") ~ "middle",
                                  TRUE ~ "unk"),
         
         
         run_gap = case_when(!is.na(run_gap) ~ run_gap,
                             run_location == "middle" ~ "guard",
                             str_detect(desc, " end") ~ "end",
                             str_detect(desc, " tackle") ~ "tackle",
                             str_detect(desc, " guard") ~ "guard",
                             str_detect(desc, " middle") ~ "guard",
                             TRUE ~ "unk"),
         
         temp = case_when(roof %in% c("closed", "dome") ~ 68L,
                          is.na(temp) ~ 60L,
                          TRUE ~ temp),
         wind = case_when(roof %in% c("closed", "dome") ~ 0L,
                          is.na(wind) ~ 8L,
                          TRUE ~ wind),
         
         rushing_fantasy_points = 6*rush_touchdown  + 2*two_point_converted + 0.1*yards_gained - 2*fumble_lost,
         
         run_gap_dir = paste(run_location, run_gap, sep = "_")) %>%
  
  filter(run_gap_dir %in% c("left_end", "left_tackle", "left_guard", "middle_guard",
                            "right_guard", "right_tackle", "right_end")) %>% 
  select(season,
         week,
         
         # desc,
         # player_name,
         # posteam,
         
         posteam_type,
         game_month,
         game_week,
         game_wday,
         game_half,
         run_location,
         run_gap,
         run_gap_dir,
         surface,
         wind,
         temp,
         roof,
         # Active_Inactive,
         # Game_Designation,
         # Injury_Type,
         position,
         
         rusher_age,
         game_number,
         game_number_active,
         
         yards_gained,
         yardline_100,
         quarter_seconds_remaining,
         half_seconds_remaining,
         game_seconds_remaining,
         drive,
         drive_play_count,
         # drive_start_yard_line,
         
         qtr,
         down,
         goal_to_go,
         ydstogo,
         shotgun,
         no_huddle,
         qb_dropback,
         qb_scramble,
         score_differential,
         ep,
         wp,
         vegas_wp,
         two_point_attempt,
         series,
         total_line,
         
         contains("to_date"),
         contains("rolling16"),
         -contains("snap"),
         -contains("broken"),
         -contains("pass"),
         -contains("interceptions"),
         -contains("completions"),
         -contains("sack"),
         -contains("rec"),
         -starts_with("attempts")
  )


rm(rolling_df)


## Train Test Split Data 

set.seed(815)

rushing_train <-
  rushing_df %>%
  filter(season <= 2019)

training_resamples <- 
  rushing_train %>% 
  vfold_cv(v = 5)
# nest(cols = -season) %>%
# rolling_origin(
#   initial = 3,
#   assess = 1,
#   cumulative = FALSE) %>%
# mutate(splits = map(splits, ~unnest(.x$data, cols = c(cols))))

rushing_test <-
  rushing_df %>%
  filter(season > 2019)









best_results <- 
  mars_xgb_set %>% 
  pull_workflow_set_result("normalized_xgboost") %>% 
  select_best(metric = "rmse")

boosting_test_results <- 
  mars_xgb_set %>% 
  pull_workflow("normalized_xgboost") %>% 
  finalize_workflow(best_results) %>% 
  fit(data = rushing_train)


mario_explainer <- 
  explain_tidymodels(
    boosting_test_results,
    data = dplyr::select(rushing_train, -yards_gained),
    y = as.integer(rushing_train$yards_gained),
    verbose = TRUE
  )

# predict_parts(mario_explainer, 
#               new_observation = rushing_train %>% sample_n(1),
#               type = "break_down")

plot(break_down(mario_explainer, new_observation = rushing_train %>% sample_n(1)))

plot(feature_importance(mario_explainer))

pdp_time <- 
  model_profile(
    mario_explainer,
    variables = "vegas_wp",
    groups = "qtr"
  )

pdp_time <- 
  model_profile(
    mario_explainer,
    variables = "ep"
  )

plot(pdp_time) + xlim(2,7)

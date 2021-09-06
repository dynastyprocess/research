library(tidyverse)
library(tidymodels)
library(earth)
library(here)
library(arrow)
library(furrr)
library(vip)
library(slider)
# remotes::install_version("hardhat", version = "0.1.4")
load(file = "archive/analysis/new_models.rda")

get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}

get_rate <- function(x,y){
  rate <- sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE)
  
  ifelse(is.nan(rate) | is.infinite(rate), 0, rate)
}


nflfastr_rosters <-
  nflfastR::fast_scraper_roster(1999:2020) %>%
  select(season, gsis_id, position, full_name, birth_date, sportradar_id) %>% 
  mutate(position = dplyr::if_else(position %in% c("HB","FB"), "RB", position))

rushing_df_test <-
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/nflfastr_pbp") %>% 
  filter(season >= 2001) %>% 
  dplyr::collect() %>%
  filter(play_type == "run",
         !str_detect(desc, "kneel|Aborted")) %>%
  left_join(nflfastr_rosters, by = c("fantasy_player_id" = "gsis_id", "season")) %>%
  # inner_join(rolling_df, by = c("fantasy_player_id"="gsis_id", "season", "week")) %>%
  filter(position %in% c("QB","RB","WR","TE")) %>% 
  mutate(rusher_age = get_age(birth_date, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         yards_gained = ifelse(two_point_attempt == 1 & two_point_converted == 1, yardline_100, yards_gained),
         down = ifelse(two_point_attempt == 1, 5, down),
         temp = case_when(roof %in% c("closed", "dome") ~ 68L,
                          is.na(temp) ~ 60L,
                          TRUE ~ temp),
         wind = case_when(roof %in% c("closed", "dome") ~ 0L,
                          is.na(wind) ~ 8L,
                          TRUE ~ wind),
         rushFP = 6*rush_touchdown  + 2*two_point_converted + 0.1*yards_gained - 2*fumble_lost,
         score = ifelse(rush_touchdown == 1 | two_point_converted == 1, 1, 0),
         run_gap = ifelse(is.na(run_gap), "center", as.character(run_gap)),
         run_location = ifelse(is.na(run_location), "unknown", as.character(run_location)),
         run_gap_dir = paste0(run_location,run_gap),
         run_gap_dir = case_when(run_gap_dir %in% c("leftcenter","rightcenter","middleend") | run_location == "unknown" ~ "unknown",
                                 TRUE ~ run_gap_dir),
         #season = as.factor(season),
         game_id = as.factor(game_id)) %>%
  
  arrange(fantasy_player_id, game_id) %>%
  group_by(full_name, fantasy_player_id) %>%
  mutate(ypc_ToDate = slide2_dbl(yards_gained, rush_attempt, ~get_rate(.x,.y), .before = Inf, .after = -1)) %>%
  ungroup()%>%
  
  cbind(predict(RushYDmod, new_data= .)) %>%
  rename(eRushYD = .pred) %>%
  cbind(predict(RushTDmod, new_data= ., type="prob")) %>%
  rename(eRushTD = .pred_1) %>%
  mutate(eRushTD = ifelse(eRushTD < 0, 0, eRushTD)) %>%
  cbind(predict(RushFPmod, new_data= .)) %>%
  rename(eRushFP = .pred) %>%
  select(season, week, posteam, game_id, fantasy_player_id, full_name, position,
         yards_gained, rushFP, rush_touchdown, two_point_converted, eRushYD, eRushTD, eRushFP)

rushing_df_test %>% 
  mutate(test_val = 4.35787) %>% 
  filter(season == 2020) %>% 
  rmse(yards_gained, test_val)

rushing_df_test %>% 
  # mutate(test_val = 4.35787) %>% 
  filter(season == 2020) %>% 
  mn_log_loss(score, eRushTD)

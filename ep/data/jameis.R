# Libraries ----------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(earth)
library(here)
library(arrow)
library(furrr)
library(vip)
library(slider)

# Raw Data ----------------------------------------------------------------
set.seed(1234)
memory.limit(size=30000)
doParallel::registerDoParallel()

setwd(here())

load(file = "models/new_models.rda")
rosters <- read_parquet("data/rosters/rosters_1999_2019.pdata")
pbp <- read_parquet("data/pbp_data/pbp_reg_post_1999_2019.pdata")

# Functions ---------------------------------------------------------------
get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}

passdf <- pbp %>% 
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          receiver_gsis_name = Player,
                          receiver_gsis_bday = Birth_date,
                          receiver_gsis_pos = Position),
            by = c("receiver_player_id" = "GSIS_ID")) %>%
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          passer_gsis_name = Player,
                          passer_gsis_bday = Birth_date,
                          passer_gsis_pos = Position),
            by = c("passer_player_id" = "GSIS_ID")) %>%
  filter(play_type == "pass",
         sack == 0,
         #season >= 2006,
         #two_point_attempt == 0,
         #!is.na(air_yards),
         !is.na(receiver_gsis_pos),
         !is.na(passer_gsis_pos),
         !is.na(alt_game_id)) %>%
  mutate(passer_age = get_age(passer_gsis_bday, game_date, dec = TRUE),
         receiver_age = get_age(receiver_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         air_yards = ifelse(two_point_attempt == 1, yardline_100, air_yards),
         down = ifelse(two_point_attempt == 1, 5, down),
         complete_pass = ifelse(two_point_attempt == 1 & grepl("is complete", desc), 1, complete_pass),
         yards_gained = ifelse(two_point_attempt == 1 & two_point_converted == 1, yardline_100, yards_gained),
         temp = case_when(roof == "closed" | roof == "dome" ~ 68,
                          is.na(temp) ~ 60,
                          TRUE ~ wind),
         wind = case_when(roof == "closed" | roof == "dome" ~ 0,
                          is.na(wind) ~ 8,
                          TRUE ~ wind),         
         recFP = 6*pass_touchdown + 2*two_point_converted  + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         passFP =  4*pass_touchdown + 2*two_point_converted  + 0.04*yards_gained - 2*fumble_lost - 2*interception,
         score = ifelse(pass_touchdown == 1 | two_point_converted == 1, 1, 0),
         pass_location = ifelse(is.na(pass_location), "unknown", as.character(pass_location)),
         air_yards = ifelse(air_yards < -10, 0, air_yards),
         air_is_zero = ifelse(air_yards==0,1,0),
         targetline = yardline_100 - air_yards,
         season = as.factor(season),
         game_id = as.factor(game_id))

pass2019 <- passdf %>%
  filter(season == 2019, wp <= 0.2, interception == 1) %>%
  group_by(passer_gsis_name) %>%
  tally()

pass2019 <- passdf %>%
  filter(season == 2019, game_half == "Half1") %>%
  group_by(passer_gsis_name) %>%
  summarise(yards = sum(yards_gained, na.rm = TRUE))
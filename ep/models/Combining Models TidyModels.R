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

# Rush Data ---------------------------------------------------------------
rushdf <- pbp %>%
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          rusher_gsis_name = Player,
                          rusher_gsis_bday = Birth_date,
                          rusher_gsis_pos = Position),
            by = c("rusher_player_id" = "GSIS_ID")) %>%
  filter(play_type == "run",
         !grepl("kneel",desc),
         !is.na(yards_gained),
         !is.na(rusher_gsis_pos),
         !is.na(alt_game_id)) %>%
  mutate(rusher_age = get_age(rusher_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         yards_gained = ifelse(two_point_attempt == 1 & two_point_converted == 1, yardline_100, yards_gained),
         down = ifelse(two_point_attempt == 1, 5, down),
         temp = case_when(roof == "closed" | roof == "dome" ~ 68,
                          is.na(temp) ~ 60,
                          TRUE ~ wind),
         wind = case_when(roof == "closed" | roof == "dome" ~ 0,
                          is.na(wind) ~ 8,
                          TRUE ~ wind),  
         rushFP = 6*rush_touchdown  + 2*two_point_converted + 0.1*yards_gained - 2*fumble_lost,
         score = ifelse(rush_touchdown == 1 | two_point_converted == 1, 1, 0),
         run_gap = ifelse(is.na(run_gap), "center", as.character(run_gap)),
         run_location = ifelse(is.na(run_location), "unknown", as.character(run_location)),
         run_gap_dir = paste0(run_location,run_gap),
         run_gap_dir = case_when(run_gap_dir %in% c("leftcenter","rightcenter","middleend") | run_location == "unknown" ~ "unknown",
                                 TRUE ~ run_gap_dir),
         season = as.factor(season),
         game_id = as.factor(game_id)) %>%
  cbind(predict(RushYDmod, new_data= .)) %>%
  rename(eRushYD = .pred) %>%
  cbind(predict(RushTDmod, new_data= ., type="prob")) %>%
  rename(eRushTD = .pred_1) %>%
  mutate(eRushTD = ifelse(eRushTD < 0, 0, eRushTD)) %>%
  cbind(predict(RushFPmod, new_data= .)) %>%
  rename(eRushFP = .pred) %>%
  select(season, game_id, rusher_player_id, rusher_gsis_name, rusher_gsis_pos,
         yards_gained, rushFP, rush_touchdown, two_point_converted, eRushYD, eRushTD, eRushFP)

# Pass Data ---------------------------------------------------------------
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
         game_id = as.factor(game_id)) %>%
  cbind(predict(Recmod, new_data= ., type="prob")) %>%
  rename(eRec = .pred_1, neRec = .pred_0) %>%
  cbind(predict(RecYDmod, new_data= .)) %>%
  rename(eRecYDs = .pred) %>%
  cbind(predict(RecTDmod, new_data= ., type="prob")) %>%
  rename(eRecTDs = .pred_1, neRecTD = .pred_0) %>%
  mutate(eRecTDs = ifelse(eRecTDs < 0, 0, eRecTDs)) %>%
  cbind(predict(RecFPmod, new_data= .)) %>%
  rename(eRecFP = .pred) %>%
  cbind(predict(PassFPmod, new_data= .)) %>%
  rename(ePassFP = .pred) %>%
  select(season, game_id, receiver_player_id, receiver_gsis_name, receiver_gsis_pos, passer_player_id, passer_gsis_name, passer_gsis_pos,
         passFP, recFP, yards_gained, pass_touchdown, air_yards, complete_pass, eRec, eRecYD = eRecYDs, eRecTD = eRecTDs, eRecFP, ePassFP)

#rm(pbp)

# Combine Rushing and Passing ---------------------------------------------
rushGame <- rushdf %>%
  group_by(season, game_id, rusher_player_id, rusher_gsis_name, rusher_gsis_pos) %>%
  summarise(across(is.numeric, sum, na.rm = TRUE),
            Carries = n()) %>%
  ungroup() %>%
  select(season, game_id, rusher_player_id, rusher_gsis_name, rusher_gsis_pos,
         rushFP, rushYD = yards_gained, Carries, rushTD = rush_touchdown, eRushYD, eRushTD, eRushFP)

recGame <- passdf %>%
  group_by(season, game_id, receiver_player_id, receiver_gsis_name, receiver_gsis_pos) %>%
  summarise(across(is.numeric, sum, na.rm = TRUE),
            Targets = n()) %>%
  ungroup() %>%
  select(season, game_id, receiver_player_id, receiver_gsis_name, receiver_gsis_pos,
         recFP, recYD = yards_gained, recTD = pass_touchdown, AirYards = air_yards, Targets, Rec = complete_pass, eRec, eRecYD, eRecFP)

passGame <- passdf %>%
  group_by(season, game_id, passer_player_id, passer_gsis_name, passer_gsis_pos) %>%
  summarise(across(is.numeric, sum, na.rm = TRUE),
            Attempts = n()) %>%
  ungroup() %>%
  select(season, game_id, passer_player_id, passer_gsis_name, passer_gsis_pos,
         passFP, passYD = yards_gained, passTD = pass_touchdown, Attempts,
         Completions = complete_pass, ePassYD = eRecYD, ePassTD = eRecTD, ePassFP)

all_games <- 
  full_join(rushGame, recGame,  by=c("game_id", "season", "rusher_player_id" = "receiver_player_id")) %>%
  mutate(combo_id = ifelse(is.na(rusher_player_id), receiver_player_id, rusher_player_id),
         combo_name = ifelse(is.na(rusher_gsis_name), receiver_gsis_name, rusher_gsis_name),
         combo_pos = ifelse(is.na(rusher_gsis_pos), receiver_gsis_pos, rusher_gsis_pos)) %>%
  full_join(passGame, by=c("game_id", "season", "combo_id" = "passer_player_id")) %>%
  mutate(player_id = ifelse(is.na(combo_id), passer_player_id, combo_id),
         gsis_name = ifelse(is.na(combo_name), passer_gsis_name, combo_name),
         gsis_pos = ifelse(is.na(combo_pos), passer_gsis_pos, combo_pos)) %>%
  rowwise() %>%
  mutate(TotalFP = sum(passFP, rushFP, recFP, na.rm = TRUE),
         eTotalFP = sum(ePassFP, eRushFP, eRecFP, na.rm = TRUE),
         TotalFPDiff = TotalFP - eTotalFP) %>%
  ungroup() %>%
  select(season, game_id, player_id, gsis_name, gsis_pos, is.numeric)

# Rolling Averages ---------------------------------------------
games_slide <- all_games %>%
  filter(substr(game_id,1,4) >= 2006) %>%
  #filter(gsis_name == "Christian Kirk") %>%
  mutate(game_id = as.factor(game_id)) %>%
  arrange(player_id, game_id) %>%
  group_by(player_id, gsis_name, gsis_pos) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(#AvgToDate = across(is.numeric, ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1)),
         #AvgPrev2 = across(is.numeric, ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1, .complete = TRUE)),
    
         #Expected Points
         AvgEP_ToDate = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgEP_Prev2 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgEP_Prev4 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgEP_Prev6 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgEP_Prev8 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgEP_Prev10 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgEP_Prev12 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgEP_Prev14 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgEP_Prev16 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgEP_Prev18 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgEP_Prev20 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgEP_Prev22 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgEP_Prev24 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgEP_Prev26 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgEP_Prev28 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgEP_Prev30 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgEP_Prev32 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         AvgRushEP_ToDate = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgRushEP_Prev2 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgRushEP_Prev4 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgRushEP_Prev6 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgRushEP_Prev8 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgRushEP_Prev10 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgRushEP_Prev12 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgRushEP_Prev14 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgRushEP_Prev16 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgRushEP_Prev18 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgRushEP_Prev20 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgRushEP_Prev22 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgRushEP_Prev24 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgRushEP_Prev26 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgRushEP_Prev28 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgRushEP_Prev30 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgRushEP_Prev32 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         AvgRecEP_ToDate = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgRecEP_Prev2 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgRecEP_Prev4 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgRecEP_Prev6 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgRecEP_Prev8 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgRecEP_Prev10 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgRecEP_Prev12 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgRecEP_Prev14 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgRecEP_Prev16 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgRecEP_Prev18 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgRecEP_Prev20 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgRecEP_Prev22 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgRecEP_Prev24 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgRecEP_Prev26 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgRecEP_Prev28 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgRecEP_Prev30 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgRecEP_Prev32 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         AvgPassEP_ToDate = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgPassEP_Prev2 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgPassEP_Prev4 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgPassEP_Prev6 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgPassEP_Prev8 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgPassEP_Prev10 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgPassEP_Prev12 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgPassEP_Prev14 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgPassEP_Prev16 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgPassEP_Prev18 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgPassEP_Prev20 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgPassEP_Prev22 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgPassEP_Prev24 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgPassEP_Prev26 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgPassEP_Prev28 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgPassEP_Prev30 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgPassEP_Prev32 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         #AirYards
         AvgAY_ToDate = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgAY_Prev2 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgAY_Prev4 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgAY_Prev6 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgAY_Prev8 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgAY_Prev10 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgAY_Prev12 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgAY_Prev14 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgAY_Prev16 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgAY_Prev18 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgAY_Prev20 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgAY_Prev22 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgAY_Prev24 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgAY_Prev26 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgAY_Prev28 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgAY_Prev30 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgAY_Prev32 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         #Fantasy Points
         AvgFP_ToDate = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgFP_Prev2 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgFP_Prev4 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgFP_Prev6 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgFP_Prev8 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgFP_Prev10 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgFP_Prev12 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgFP_Prev14 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgFP_Prev16 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgFP_Prev18 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgFP_Prev20 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgFP_Prev22 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgFP_Prev24 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgFP_Prev26 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgFP_Prev28 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgFP_Prev30 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgFP_Prev32 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         AvgRecFP_ToDate = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgRecFP_Prev2 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgRecFP_Prev4 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgRecFP_Prev6 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgRecFP_Prev8 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgRecFP_Prev10 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgRecFP_Prev12 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgRecFP_Prev14 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgRecFP_Prev16 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgRecFP_Prev18 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgRecFP_Prev20 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgRecFP_Prev22 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgRecFP_Prev24 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgRecFP_Prev26 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgRecFP_Prev28 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgRecFP_Prev30 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgRecFP_Prev32 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         AvgRushFP_ToDate = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgRushFP_Prev2 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgRushFP_Prev4 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgRushFP_Prev6 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgRushFP_Prev8 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgRushFP_Prev10 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgRushFP_Prev12 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgRushFP_Prev14 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgRushFP_Prev16 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgRushFP_Prev18 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgRushFP_Prev20 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgRushFP_Prev22 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgRushFP_Prev24 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgRushFP_Prev26 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgRushFP_Prev28 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgRushFP_Prev30 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgRushFP_Prev32 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         AvgPassFP_ToDate = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
         AvgPassFP_Prev2 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
         AvgPassFP_Prev4 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
         AvgPassFP_Prev6 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
         AvgPassFP_Prev8 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
         AvgPassFP_Prev10 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
         AvgPassFP_Prev12 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
         AvgPassFP_Prev14 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
         AvgPassFP_Prev16 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
         AvgPassFP_Prev18 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
         AvgPassFP_Prev20 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
         AvgPassFP_Prev22 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
         AvgPassFP_Prev24 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
         AvgPassFP_Prev26 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
         AvgPassFP_Prev28 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
         AvgPassFP_Prev30 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
         AvgPassFP_Prev32 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
         
         ROCAvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = Inf),
         Next4_AvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = 4),
         Next8_AvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = 8),
         Next12_AvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = 12),
         Next16_AvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = 16),
         
  ) %>%
  ungroup()



# Mars Model ---------------------------------------------
fp_model <- function(df){
  earth(Next16_AvgFP ~ ., data = df, degree = 2)
}


slide_reg <- games_slide %>%
  filter(!is.na(ROCAvgFP)) %>%
  select(Next16_AvgFP, gsis_pos, starts_with("Avg")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(gsis_pos = as.factor(gsis_pos)) %>%
  na.omit()

sliding_split <- initial_split(slide_reg, prop = 4/5)

# formulas <- c("RocAvgFP ~ ., data = df",
#               #"Next4_AvgFP ~ ., data = df",
#               "Next8_AvgFP ~ ., data = df",
#               #"Next12_AvgFP ~ ., data = df",
#               "Next16_AvgFP ~ ., data = df")

slidedf_train <- training(sliding_split) %>%
  group_by(gsis_pos) %>%
  nest() %>%
  mutate(model = future_map(data, fp_model),
         summs = future_map(model, summary),
         vips = future_map(model, vip))


slidedf_train$gsis_pos[[3]]
slidedf_train$summs[[3]]

slidedf_train$vips[[3]]

pdp::partial(slidedf_train$model[[3]], pred.var = "AvgFP_Prev28", train = slidedf_train$data[[3]]) %>% autoplot()
pdp::partial(slidedf_train$model[[4]], pred.var = c("AvgFP_Prev14","AvgEP_Prev4"), train = slidedf_train$data[[4]]) %>% autoplot()

temp <- slidedf_train$data[[3]]
temp2 <- games_slide %>% filter(AvgAY_ToDate < 0, gsis_pos == "QB") %>% arrange(-ROCAvgFP)

qbpred <- games_slide %>%
  filter(gsis_pos == "QB")
  
qbpred <- qbpred %>%
  bind_cols(pred = predict(slidedf_train$model[[3]], newdata = qbpred)) %>%
  unnest(cols = c(pred)) %>%
  rename(Next16Pred = pred) %>%
  select(is.character, Next16Pred, game_id)

LatestPrediction <- qbpred %>%
  filter(substr(game_id,1,4) == 2018) %>%
  group_by(player_id) %>%
  mutate(game_id = as.character(game_id),
          maxgame_id = max(game_id)) %>%
  ungroup() %>%
  dplyr::select(player_id, maxgame_id) %>%
  distinct()

QBs2020 <- qbpred %>%
  inner_join(LatestPrediction, by = c("player_id","game_id" = "maxgame_id"))

# vars <- games_slide %>% select(contains("Pass")) %>% names()
# 
# lm(as.formula(Next16_AvgFP ~ vars), data = games_slide)

# Libraries ----------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(here)
library(arrow)
library(furrr)
library(vip)

library(skimr)
library(pdp)

# Raw Data ----------------------------------------------------------------
set.seed(1234)
memory.limit(size=30000)
doParallel::registerDoParallel()

setwd(here())
rosters <- read_parquet("data/rosters/rosters_1999_2019.pdata")
pbp <- read_parquet("data/pbp_data/pbp_reg_post_1999_2019.pdata")
#temp <- pbp %>% filter(two_point_attempt == 1) %>% select(desc, play_type, complete_pass, two_point_conv_result, yardline_100, yards_gained)
#pbp %>% group_by(play_type,two_point_attempt) %>% tally()

# Functions ---------------------------------------------------------------
get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}

get_rate <- function(x,y){
  sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE)
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
                                 TRUE ~ run_gap_dir)) %>%
  
  group_by(game_id, posteam) %>%
  mutate(team_rush_yards = slide_dbl(yards_gained, ~sum(.x, na.rm = TRUE), .before = Inf, .after = 0),
         team_attempts = slide_dbl(rush_attempt, ~sum(.x, na.rm = TRUE), .before = Inf, .after = 0)) %>%
  ungroup() %>%
  
  arrange(rusher_player_id, game_id) %>%
  group_by(rusher_gsis_name, rusher_player_id) %>%
  mutate(new_game_flag = ifelse(lag(game_id) == game_id, 0, 1),
         new_game_flag = ifelse(is.na(new_game_flag),1,new_game_flag),
         
         new_team_yards = ifelse(new_game_flag == 1, team_rush_yards, team_rush_yards - lag(team_rush_yards)), 
         new_team_attempts = ifelse(new_game_flag == 1, team_attempts, team_attempts - lag(team_attempts)), 
         
         rush_yards_share_ToDate = slide2_dbl(yards_gained, new_team_yards, ~get_rate(.x,.y), .before = Inf, .after = 0),
         rush_yards_share_ToDate = case_when(is.nan(rush_yards_share_ToDate) | is.infinite(rush_yards_share_ToDate) | rush_yards_share_ToDate < 0 ~ 0,
                                             rush_yards_share_ToDate > 1 ~ 1,
                                             TRUE ~ rush_yards_share_ToDate),
         
         ypc_ToDate = slide2_dbl(yards_gained, new_team_yards, ~get_rate(.x,.y), .before = Inf, .after = 0),
         
         attempt_share_ToDate = slide2_dbl(rush_attempt, new_team_attempts, ~get_rate(.x,.y), .before = Inf, .after = 0),
         attempts_ToDate = row_number()) %>%
  ungroup() %>%
  
  select(season, yards_gained, score, rushFP, rush_touchdown, two_point_converted, yardline_100, qb_scramble, run_gap_dir,
         run_gap, shotgun, rusher_gsis_pos, ydstogo, two_point_attempt, attempts_ToDate, attempt_share_ToDate, rush_yards_share_ToDate,
         rusher_gsis_name, alt_game_id, posteam, team_rush_yards, team_attempts) %>%
  mutate_if(is.character, as.factor) %>%
  na.omit()

rm(pbp)

# Train Test Split Data ---------------------------------------------------
rushdf_split <- initial_split(rushdf, prop = 4/5, strata = season)

rushdf_train <- training(rushdf_split)
folds <- vfold_cv(rushdf_train, 4)
rushdf_test <- testing(rushdf_split)

# Rushing Yards ,---------------------------------------------------------
rushyds_mars <- mars(
  num_terms = 10, #tune(),
  prod_degree = 2,
  prune_method = "exhaustive") %>%
  set_mode("regression") %>%
  set_engine("earth")

rushyds_wf <- workflow() %>%
  add_model(rushyds_mars) %>%
  add_formula(yards_gained ~  yardline_100 + qb_scramble + run_gap_dir + run_gap + shotgun + rusher_gsis_pos + attempts_ToDate + attempt_share_ToDate + rush_yards_share_ToDate)

# rushyds_grid <- grid_regular(
#   num_terms(range = c(6,9)),
#   levels = 3
# )
# 
# rushyds_tune <- rushyds_wf %>%
#   tune_grid(resamples = folds,
#             grid = rushyds_grid)
# 
# rushyds_tune %>%
#   collect_metrics %>%
#   filter(.metric == "rsq") %>%
#   ggplot(aes(num_terms, mean))+
#   geom_point()
# 
# rushyds_wf <- rushyds_wf %>%
#   finalize_workflow(tibble(num_terms = 8))

rushyds_fit <- fit(rushyds_wf, data = rushdf_train)

pull_workflow_fit(rushyds_fit) %>%
  vip(geom = "point")

rushyds_earthmodel <- rushyds_fit$fit$fit$fit
rushyds_earthdata <- rushyds_fit$pre$mold$predictors

summary(rushyds_earthmodel)
#pdp::partial(rushyds_earthmodel, pred.var = c("rush_yards_share_ToDate"), train = rushyds_earthdata) %>% autoplot() + geom_rug()
#pdp::partial(rushyds_earthmodel, pred.var = c("rush_yards_share_ToDate","attempt_share_ToDate"), train = rushyds_earthdata) %>% autoplot() + geom_rug()

rushyds_fit %>%
  predict(new_data= rushdf_test) %>%
  mutate(yards_gained = rushdf_test$yards_gained) %>%
  #summarise(mean(yards_gained), mean(.pred))
  rsq(yards_gained, .pred)

rushdf_train <- rushdf_train %>%
  cbind(pred = predict(rushyds_fit, new_data= rushdf_train)) %>%
  rename(eRushYD = .pred)

rushdf_test <- rushdf_test %>%
  cbind(pred = predict(rushyds_fit, new_data= rushdf_test)) %>%
  rename(eRushYD = .pred)

# Rushing Scores ---------------------------------------------------------
folds <- vfold_cv(rushdf_train, 4)

rushtds_mars <- mars(
  num_terms = tune(),
  prod_degree = 2,
  prune_method = "exhaustive") %>%
  set_mode("classification") %>%
  set_engine("earth")

rushtds_wf <- workflow() %>%
  add_model(rushtds_mars) %>%
  add_formula(as.factor(score) ~  yardline_100 + eRushYD + two_point_attempt + rusher_gsis_pos + run_gap + ydstogo)

# rushtds_grid <- grid_regular(
#   num_terms(range = c(6,15)),
#   levels = 4
# )
# 
# rushtds_tune <- rushtds_wf %>%
#   tune_grid(resamples = folds,
#             grid = rushtds_grid)
# 
# rushtds_tune %>%
#   collect_metrics %>%
#   filter(.metric == "rsq") %>%
#   ggplot(aes(num_terms, mean))+
#   geom_point()

rushtds_wf <- rushtds_wf %>%
  finalize_workflow(tibble(num_terms = 12))

rushtds_fit <- fit(rushtds_wf, data = rushdf_train)

pull_workflow_fit(rushtds_fit) %>%
  vip(geom = "point")

#rushtds_earthmodel <- rushtds_fit$fit$fit$fit
#rushtds_earthdata <- rushtds_fit$pre$mold$predictors

#summary(rushtds_earthmodel)
#pdp::partial(rushtds_earthmodel, pred.var = "down", train = rushtds_earthdata) %>% autoplot()

rushtds_fit %>%
  predict(new_data= rushdf_test, type = "prob") %>%
  mutate(score = rushdf_test$score) %>%
  #summarise(mean(score), mean(.pred_1))
  rsq(score, .pred_1)

rushdf_train <- rushdf_train %>%
  cbind(pred = predict(rushtds_fit, new_data= rushdf_train, type = "prob")) %>%
  rename(eRushTD = pred..pred_1)

rushdf_test <- rushdf_test %>%
  cbind(pred = predict(rushtds_fit, new_data= rushdf_test, type = "prob")) %>%
  rename(eRushTD = pred..pred_1)

# Rushing FP ---------------------------------------------------------
folds <- vfold_cv(rushdf_train, 4)

rushfps_mars <- mars(
  num_terms = tune(),
  prod_degree = 2,
  prune_method = "forward") %>%
  set_mode("regression") %>%
  set_engine("earth")#, varmod.method = "lm", nfold=4, ncross=3, Get.leverages = TRUE)

rushfps_wf <- workflow() %>%
  add_model(rushfps_mars) %>%
  add_formula(rushFP ~ eRushTD + eRushYD)

# rushfps_grid <- grid_regular(
#   num_terms(range = c(5,15)),
#   levels = 3
# )
# 
# rushfps_tune <- rushfps_wf %>%
#   tune_grid(resamples = folds,
#             grid = rushfps_grid)
# 
# rushfps_tune %>%
#   collect_metrics %>%
#   filter(.metric == "rsq") %>%
#   ggplot(aes(num_terms, mean))+
#   geom_point()

rushfps_wf <- rushfps_wf %>%
  finalize_workflow(tibble(num_terms = 5))

rushfps_fit <- fit(rushfps_wf, data = rushdf_train)

pull_workflow_fit(rushfps_fit) %>%
  vip(geom = "point")

#rushfps_earthmodel <- rushfps_fit$fit$fit$fit
# rushfps_earthdata <- rushfps_fit$pre$mold$predictors
# 
# summary(rushfps_earthmodel)
# pdp::partial(rushfps_earthmodel, pred.var = c("eRushTD","two_point_attempt"), train = rushfps_earthdata) %>% autoplot()

rushfps_fit %>%
  predict(new_data= rushdf_test) %>%
  mutate(rushFP = rushdf_test$rushFP,
         expFP1 = ifelse(rushdf_test$two_point_attempt == 0, 6*rushdf_test$eRushTD + 0.1*rushdf_test$eRushYD,
                         2*rushdf_test$eRushTD + 0.1*rushdf_test$eRushYD)) %>%
  #summarise(mean(rushFP), mean(.pred), mean(expFP1))
  rsq(rushFP, expFP1)

rushdf_train <- rushdf_train %>%
  cbind(pred = predict(rushfps_earthmodel, new_data= rushdf_train, interval="pint", level = 0.5)) %>%
  rename(eRushFP = pred.fit, eRushFP_lwr = pred.lwr, eRushFP_upr = pred.upr)
  #rename(erushfps2 = pred.fit, cintlwr = pred.lwr, cintupr = pred.upr)
  
rushdf_test <- rushdf_test %>%
  cbind(pred = predict(rushfps_fit, new_data= rushdf_test)) %>%
  rename(erushfps = .pred)


# Saving models -----------------------------------------------------------

cleanModel <- function(mod){
  mod$fit$fit$fit$bx <- NULL
  
  mod$fit$fit$fit$glm.list <- NULL
  mod$fit$fit$fit$call <- NULL
  mod$fit$fit$spec$eng_args <- NULL
  mod$fit$fit$spec$method$fit$args <- NULL
  mod
}

rushfps_earthmodel <- rushfps_fit$fit$fit$fit

temp <- rushtds_fit
temp$fit$fit$fit$glm.list <- NULL
temp$fit$fit$fit$call <- NULL
temp$fit$fit$spec$eng_args <- NULL
temp$fit$fit$spec$method$fit$args <- NULL

weigh(temp)

predict(temp, new_data= rushdf_train, type = "prob")

predict(RushTDmod, new_data= rushdf, type = "response")



RushYDmod <- cleanModel(rushyds_fit)
RushTDmod <- cleanModel(rushtds_fit)
RushFPmod <- cleanModel(rushfps_fit)

Recmod <- cleanModel(rec_fit)
RecYDmod <- cleanModel(recyds_fit)
RecTDmod <- cleanModel(rectds_fit)
RecFPmod <- cleanModel(recfps_fit)
PassFPmod <- cleanModel(passfps_fit)

save(Recmod, RecYDmod, RecTDmod, RecFPmod, PassFPmod,
     RushYDmod, RushTDmod, RushFPmod, file = "models/new_models.rda")

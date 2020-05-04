# Libraries ----------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(here)
library(arrow)
library(furrr)
library(vip)
library(slider)

library(skimr)
library(pdp)

# Raw Data ----------------------------------------------------------------
set.seed(1234)
memory.limit(size=50000)
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
         #receiver_gsis_name == "Christian Kirk",
         !is.na(receiver_gsis_pos),
         !is.na(passer_gsis_pos)) %>%
  mutate(passer_age = get_age(passer_gsis_bday, game_date, dec = TRUE),
         receiver_age = get_age(receiver_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         air_yards = ifelse(two_point_attempt == 1, yardline_100, air_yards),
         air_yards = ifelse(season < 2006, NA, air_yards),
         yards_gained = ifelse(season < 2006, NA, yards_gained),
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
         abs_air_yards = abs(air_yards),
         air_is_zero = ifelse(air_yards==0,1,0),
         targetline = yardline_100 - air_yards) %>%
  
  group_by(game_id, posteam) %>%
  mutate(team_air_yards = slide_dbl(air_yards, ~sum(.x, na.rm = TRUE), .before = Inf, .after = 0),
         team_attempts = slide_dbl(pass_attempt, ~sum(.x, na.rm = TRUE), .before = Inf, .after = 0)) %>%
  ungroup() %>%
  
  arrange(passer_player_id, game_id) %>%
  group_by(passer_gsis_name, passer_player_id) %>%
  mutate(PACR_ToDate = slide2_dbl(yards_gained, abs_air_yards, ~get_rate(.x,.y), .before = Inf, .after = -1),
         PACR_ToDate = ifelse(is.nan(PACR_ToDate) | is.infinite(PACR_ToDate), 0, PACR_ToDate),
         passes_ToDate = row_number()) %>%
  ungroup() %>%
  
  arrange(receiver_player_id, game_id) %>%
  group_by(receiver_gsis_name, receiver_player_id) %>%
  mutate(RACR_ToDate = slide2_dbl(yards_gained, abs_air_yards, ~get_rate(.x,.y), .before = Inf, .after = -1),
         RACR_ToDate = ifelse(is.nan(RACR_ToDate) | is.infinite(RACR_ToDate), 0, RACR_ToDate),
         
         new_game_flag = ifelse(lag(game_id) == game_id, 0, 1),
         new_game_flag = ifelse(is.na(new_game_flag),1,new_game_flag),
         
         new_team_airyards = ifelse(new_game_flag == 1, team_air_yards, team_air_yards - lag(team_air_yards)), 
         new_team_attempts = ifelse(new_game_flag == 1, team_attempts, team_attempts - lag(team_attempts)), 
         
         air_yards_share_ToDate = slide2_dbl(air_yards, new_team_airyards, ~get_rate(.x,.y), .before = Inf, .after = 0),
         air_yards_share_ToDate = ifelse(is.nan(air_yards_share_ToDate) | is.infinite(air_yards_share_ToDate),0,air_yards_share_ToDate),
         
         target_share_ToDate = slide2_dbl(pass_attempt, new_team_attempts, ~get_rate(.x,.y), .before = Inf, .after = 0),
         targets_ToDate = row_number()) %>%
  ungroup() %>%
  
  select(game_id, season, week, posteam_type, passer_gsis_pos, passer_age, score, two_point_converted,
         receiver_player_id, passer_player_id, receiver_gsis_name, passer_gsis_name,
         PACR_ToDate, passes_ToDate, RACR_ToDate, targets_ToDate, air_yards_share_ToDate, target_share_ToDate,
         receiver_gsis_pos, receiver_age, two_point_attempt, down, temp,
         passFP, recFP, yards_gained, pass_touchdown, yardline_100, air_yards, qb_hit,
         pass_location, targetline, shotgun, complete_pass, ydstogo, drive, goal_to_go,
         no_huddle, wind, roof, surface, wp, air_is_zero, game_seconds_remaining, half_seconds_remaining, game_half)%>%
  mutate_if(is.character, as.factor) %>%
  na.omit()

rm(pbp)


skim(passdf)

passdf %>% ggplot(aes(RACR_ToDate,complete_pass)) +geom_hex() + geom_smooth() + xlim(0,2) + ylim(0,1)
passdf %>% ggplot(aes(PACR_ToDate,complete_pass)) +geom_hex() + geom_smooth() + xlim(0.5,1.5) + ylim(0,1)

passdf %>% head(100) %>% ggplot(aes(air_yards,RACR_ToDate)) + geom_contour(aes(z = recFP)) + xlim(-5,30) + ylim(0,2)
passdf %>% head(1000) %>% ggplot(aes(air_yards,RACR_ToDate, color =recFP)) + geom_raster() + xlim(-5,30) + ylim(0,2)

# temp <- passdf %>% mutate(dome = ifelse(roof=="dome",1,0)) %>% group_by(dome, posteam_type) %>% summarise(mean(complete_pass), n())
# temp <- passdf %>% mutate(dome = ifelse(roof=="dome" & posteam_type=="home",1,0)) %>% group_by(season, dome) %>% summarise(cp = mean(complete_pass), ay = mean(air_yards), n())
# 
# 
# temp %>%
#   ggplot(aes(season,cp)) +
#   geom_point(aes(color = as.factor(dome)))
# 
# temp %>%
#   ggplot(aes(season,ay)) +
#   geom_point(aes(color = as.factor(dome)))

# Train Test Split Data ---------------------------------------------------
passdf_split <- initial_split(passdf, prop = 4/5, strata = season)

passdf_train <- training(passdf_split)
folds <- vfold_cv(passdf_train, 4)
passdf_test <- testing(passdf_split)

#write_parquet(passdf_train,"data/pbp_data/passdf_train.pdata")

# Define Models -----------------------------------------------------------
rec_mars <- mars(
  num_terms = 20, #tune(),
  prod_degree = 2,
  prune_method = "exhaustive") %>%
  set_mode("classification") %>%
  set_engine("earth")

rec_wf <- workflow() %>%
  add_model(rec_mars) %>%
  add_formula(as.factor(complete_pass) ~  air_yards + targetline + air_is_zero + pass_location + down + qb_hit
                                    + PACR_ToDate + passes_ToDate + RACR_ToDate + targets_ToDate +
                                    + air_yards_share_ToDate + target_share_ToDate)

# mars_grid <- grid_regular(
#   num_terms(range = c(12,16)),
#   levels = 3
# )
# 
# tune <- rec_wf %>%
#   tune_grid(resamples = folds,
#               grid = mars_grid)
# 
# tune %>%
#   collect_metrics %>%
#   filter(.metric == "rsq") %>%
#   ggplot(aes(num_terms, mean))+
#   geom_point()

# rec_wf <- rec_wf %>%
#   finalize_workflow(tibble(num_terms = 14))

rec_fit <- fit(rec_wf, data = passdf_train)

pull_workflow_fit(rec_fit) %>%
    vip(geom = "point")

earthmodel <- rec_fit$fit$fit$fit
earthdata <- rec_fit$pre$mold$predictors
summary(earthmodel)

pdp::partial(earthmodel, pred.var = "passes_ToDate", train = earthdata) %>% autoplot() + xlim(0.5,1)
temp <- pdp::partial(earthmodel, pred.var = c("passes_ToDate","air_yards"), train = earthdata) %>% autoplot() + ylim(-5,30)

rec_fit %>%
  predict(new_data= passdf_test, type = "prob") %>%
  mutate(complete_pass = passdf_test$complete_pass) %>%
  #summarise(mean(complete_pass), mean(.pred_1))
  rsq(complete_pass, .pred_1)

cpoe_func <- function(x,y){
  mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
}

passdf_train <- passdf_train %>%
  cbind(pred = predict(rec_fit, new_data= passdf_train, type = "prob")) %>%
  rename(eRec = pred..pred_1, nerec = pred..pred_0) %>%
  arrange(passer_player_id, game_id) %>%
  group_by(passer_gsis_name, passer_player_id) %>%
  mutate(QBCPOE_ToDate = slide2_dbl(complete_pass, eRec, ~cpoe_func(.x,.y), .before = Inf, .after = -1),
         QBCPOE_ToDate = ifelse(is.nan(QBCPOE_ToDate), 0, QBCPOE_ToDate),
         passes_ToDate = row_number()) %>%
  ungroup() %>%
  arrange(receiver_player_id, game_id) %>%
  group_by(receiver_gsis_name, receiver_player_id) %>% 
  mutate(RecCPOE_ToDate = slide2_dbl(complete_pass, eRec, ~cpoe_func(.x,.y), .before = Inf, .after = -1),
         RecCPOE_ToDate = ifelse(is.nan(RecCPOE_ToDate), 0, RecCPOE_ToDate),
         targets_ToDate = row_number()) %>%
  ungroup()

passdf%>%
  filter(passer_gsis_name == "Drew Brees" | passer_gsis_name == "Tom Brady" | passer_gsis_name == "Peyton Manning", PACR_ToDate < 2) %>%
  ggplot(aes(passes_ToDate,PACR_ToDate, color = passer_gsis_name)) +
  geom_line()

passdf_train %>%
  filter(receiver_gsis_name == "Michael Gallup" | receiver_gsis_name == "Calvin Ridley" | receiver_gsis_name == "Curtis Samuel", targets_ToDate >= 10) %>%
  ggplot(aes(targets_ToDate,RecCPOE_ToDate, color = receiver_gsis_name)) +
  geom_line()

passdf %>%
  filter(receiver_gsis_name == "Michael Gallup" | receiver_gsis_name == "Calvin Ridley" | receiver_gsis_name == "Curtis Samuel", targets_ToDate >= 10) %>%
  ggplot(aes(targets_ToDate,air_yards_share_ToDate, color = receiver_gsis_name)) +
  geom_line()

passdf_test <- passdf_test %>%
  cbind(pred = predict(rec_fit, new_data= passdf_test, type = "prob")) %>%
  rename(eRec = pred..pred_1, nerec = pred..pred_0)

# Receiving Yards ---------------------------------------------------------
folds <- vfold_cv(passdf_train, 4)

recyds_mars <- mars(
  num_terms = 20, #tune(),
  prod_degree = 2,
  prune_method = "exhaustive") %>%
  set_mode("regression") %>%
  set_engine("earth")

recyds_wf <- workflow() %>%
  add_model(recyds_mars) %>%
  add_formula(yards_gained ~  air_yards + targetline + pass_location + eRec + half_seconds_remaining + wp + qb_hit
                              + PACR_ToDate + passes_ToDate + RACR_ToDate + targets_ToDate
                              + air_yards_share_ToDate + target_share_ToDate
                              + QBCPOE_ToDate + RecCPOE_ToDate)

# recyds_grid <- grid_regular(
#   num_terms(range = c(5,15)),
#   levels = 3
# )
# 
# recyds_tune <- recyds_wf %>%
#   tune_grid(resamples = folds,
#             grid = recyds_grid)
# 
# recyds_tune %>%
#   collect_metrics %>%
#   filter(.metric == "rsq") %>%
#   ggplot(aes(num_terms, mean))+
#   geom_point()
# 
# recyds_wf <- recyds_wf %>%
#   finalize_workflow(tibble(num_terms = 12))

recyds_fit <- fit(recyds_wf, data = passdf_train)

pull_workflow_fit(recyds_fit) %>%
  vip(geom = "point")

#recyds_earthmodel <- recyds_fit$fit$fit$fit
#recyds_earthdata <- recyds_fit$pre$mold$predictors

#summary(recyds_earthmodel)
#pdp::partial(recyds_earthmodel, pred.var = "RACR_ToDate", train = recyds_earthdata) %>% autoplot() + xlim(-0.5,2)
temp <- pdp::partial(recyds_earthmodel, pred.var = c("RACR_ToDate","air_yards"), train = recyds_earthdata) %>% autoplot()
temp %>% ggplot(aes(RACR_ToDate,air_yards, color = yhat)) + geom_hex(bins = 4) + xlim(0,4) + ylim(-5,30)
temp %>% autoplot() + xlim(0,4) + ylim(-5,30)


recyds_fit %>%
  predict(new_data= passdf_test) %>%
  mutate(yards_gained = passdf_test$yards_gained) %>%
  summarise(mean(yards_gained), mean(.pred))
  #rsq(yards_gained, .pred)

passdf_train <- passdf_train %>%
  cbind(pred = predict(recyds_fit, new_data= passdf_train)) %>%
  rename(eRecYDs = .pred)

passdf_test <- passdf_test %>%
  cbind(pred = predict(recyds_fit, new_data= passdf_test)) %>%
  rename(eRecYDs = .pred)

# Receiving Scores ---------------------------------------------------------
folds <- vfold_cv(passdf_train, 4)

rectds_mars <- mars(
  num_terms = 12, #tune(),
  prod_degree = 2,
  prune_method = "exhaustive") %>%
  set_mode("classification") %>%
  set_engine("earth")

rectds_wf <- workflow() %>%
  add_model(rectds_mars) %>%
  add_formula(as.factor(score) ~  targetline + yardline_100 + air_yards + eRecYDs +
                qb_hit
              + PACR_ToDate + passes_ToDate + RACR_ToDate + targets_ToDate
              + air_yards_share_ToDate + target_share_ToDate
              + QBCPOE_ToDate + RecCPOE_ToDate)

# rectds_grid <- grid_regular(
#   num_terms(range = c(5,15)),
#   levels = 5
# )
# 
# rectds_tune <- rectds_wf %>%
#   tune_grid(resamples = folds,
#             grid = rectds_grid)
# 
# rectds_tune %>%
#   collect_metrics %>%
#   filter(.metric == "rsq") %>%
#   ggplot(aes(num_terms, mean))+
#   geom_point()
# 
# rectds_wf <- rectds_wf %>%
#   finalize_workflow(tibble(num_terms = 11))

rectds_fit <- fit(rectds_wf, data = passdf_train)

pull_workflow_fit(rectds_fit) %>%
  vip(geom = "point")

#rectds_earthmodel <- rectds_fit$fit$fit$fit
#rectds_earthdata <- rectds_fit$pre$mold$predictors

#summary(rectds_earthmodel)
#pdp::partial(rectds_earthmodel, pred.var = c("QBCPOE_ToDate"), train = rectds_earthdata) %>% autoplot()
#pdp::partial(rectds_earthmodel, pred.var = c("QBCPOE_ToDate","targetline"), train = rectds_earthdata) %>% autoplot()

# rectds_fit %>%
#   predict(new_data= passdf_test) %>%
#   mutate(score = passdf_test$score) %>%
#   #summarise(mean(score), mean(.pred))
#   rsq(score, .pred)

passdf_train <- passdf_train %>%
  cbind(pred = predict(rectds_fit, new_data= passdf_train, type = "prob")) %>%
  rename(eRecTDs = pred..pred_1)

passdf_test <- passdf_test %>%
  cbind(pred = predict(rectds_fit, new_data= passdf_test, type = "prob")) %>%
  rename(eRecTDs = pred..pred_1)

# Receiving FP ---------------------------------------------------------
folds <- vfold_cv(passdf_train, 4)

recfps_mars <- mars(
  num_terms = 12, #tune(),
  prod_degree = 2,
  prune_method = "forward") %>%
  set_mode("regression") %>%
  set_engine("earth")#, varmod.method = "lm", nfold=4, ncross=3, Get.leverages = TRUE)

recfps_wf <- workflow() %>%
  add_model(recfps_mars) %>%
  add_formula(recFP ~ eRec + eRecYDs + eRecTDs + two_point_attempt + qb_hit
              + PACR_ToDate + passes_ToDate + RACR_ToDate + targets_ToDate
              + air_yards_share_ToDate + target_share_ToDate
              + QBCPOE_ToDate + RecCPOE_ToDate)

# recfps_grid <- grid_regular(
#   num_terms(range = c(5,15)),
#   levels = 5
# )
# 
# recfps_tune <- recfps_wf %>%
#   tune_grid(resamples = folds,
#             grid = recfps_grid)
# 
# recfps_tune %>%
#   collect_metrics %>%
#   filter(.metric == "rsq") %>%
#   ggplot(aes(num_terms, mean))+
#   geom_point()

# recfps_wf <- recfps_wf %>%
#   finalize_workflow(tibble(num_terms = 7))

recfps_fit <- fit(recfps_wf, data = passdf_train)

pull_workflow_fit(recfps_fit) %>%
  vip(geom = "point")

recfps_earthmodel <- recfps_fit$fit$fit$fit
#recfps_earthdata <- recfps_fit$pre$mold$predictors

summary(recfps_earthmodel)
#pdp::partial(recfps_earthmodel, pred.var = c("QBCPOE_ToDate"), train = recfps_earthdata) %>% autoplot()
#pdp::partial(recfps_earthmodel, pred.var = c("QBCPOE_ToDate","eRec"), train = recfps_earthdata) %>% autoplot()


# recfps_fit %>%
#   predict(new_data= passdf_test) %>%
#   mutate(recFP = passdf_test$recFP) %>%
#   #summarise(mean(score), mean(.pred))
#   rsq(recFP, .pred)
# 
# passdf_train <- passdf_train %>%
#   cbind(pred = predict(recfps_earthmodel, new_data= passdf_train, interval="pint", level = 0.5)) %>%
#   rename(eRecFP = pred.fit, eRecFP_lwr = pred.lwr, eRecFP_upr = pred.upr)
#   #rename(erecfps2 = pred.fit, cintlwr = pred.lwr, cintupr = pred.upr)
#   
# passdf_test <- passdf_test %>%
#   cbind(pred = predict(recfps_fit, new_data= passdf_test)) %>%
#   rename(erecfps = .pred)

# passdf_train %>%
#   ggplot(aes(air_yards))+
#   #geom_point(aes(y=erecfps)) +
#   geom_smooth(aes(y=erecfps),  color = "black") +
#   geom_smooth(aes(y=cintlwr), color = "blue") +
#   geom_smooth(aes(y=cintupr), color = "blue") +
#   geom_smooth(aes(y=pintlwr4), color = "red") +
#   geom_smooth(aes(y=pintupr4), color = "red")

# Passing FP ---------------------------------------------------------
folds <- vfold_cv(passdf_train, 4)

passfps_mars <- mars(
  num_terms = tune(),
  prod_degree = 2,
  prune_method = "forward") %>%
  set_mode("regression") %>%
  set_engine("earth")#, varmod.method = "lm", nfold=4, ncross=3, Get.leverages = TRUE)

passfps_wf <- workflow() %>%
  add_model(passfps_mars) %>%
  add_formula(passFP ~ eRecTDs + eRecYDs + two_point_attempt)

# passfps_grid <- grid_regular(
#   num_terms(range = c(5,15)),
#   levels = 5
# )
# 
# passfps_tune <- passfps_wf %>%
#   tune_grid(resamples = folds,
#             grid = passfps_grid)
# 
# passfps_tune %>%
#   collect_metrics %>%
#   filter(.metric == "rsq") %>%
#   ggplot(aes(num_terms, mean))+
#   geom_point()

passfps_wf <- passfps_wf %>%
  finalize_workflow(tibble(num_terms = 7))

passfps_fit <- fit(passfps_wf, data = passdf_train)

pull_workflow_fit(passfps_fit) %>%
  vip(geom = "point")

#passfps_earthmodel <- passfps_fit$fit$fit$fit
#passfps_earthdata <- passfps_fit$pre$mold$predictors

#summary(passfps_earthmodel)
#pdp::partial(passfps_earthmodel, pred.var = c("eRecTDs"), train = passfps_earthdata) %>% autoplot()

passfps_fit %>%
  predict(new_data= passdf_test) %>%
  mutate(recFP = passdf_test$recFP) %>%
  #summarise(mean(score), mean(.pred))
  rsq(recFP, .pred)

# passdf_train <- passdf_train %>%
#   cbind(pred = predict(passfps_earthmodel, new_data= passdf_train, interval="pint", level = 0.5)) %>%
#   rename(ePassFP = pred.fit, ePassFP_lwr = pred.lwr, ePassFP_upr = pred.upr)
# #rename(epassfps2 = pred.fit, cintlwr = pred.lwr, cintupr = pred.upr)
# 
# passdf_test <- passdf_test %>%
#   cbind(pred = predict(passfps_fit, new_data= passdf_test)) %>%
#   rename(epassfps = .pred)

# passdf_train %>%
#   ggplot(aes(air_yards))+
#   #geom_point(aes(y=epassfps)) +
#   geom_smooth(aes(y=epassfps),  color = "black") +
#   geom_smooth(aes(y=cintlwr), color = "blue") +
#   geom_smooth(aes(y=cintupr), color = "blue") +
#   geom_smooth(aes(y=pintlwr4), color = "red") +
#   geom_smooth(aes(y=pintupr4), color = "red")
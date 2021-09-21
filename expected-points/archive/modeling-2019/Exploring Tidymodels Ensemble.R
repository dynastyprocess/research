# Libraries ----------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(here)
library(arrow)
library(furrr)
library(vip)

# Raw Data ----------------------------------------------------------------
set.seed(1234)
memory.limit(size=30000)
doParallel::registerDoParallel()

setwd(here())
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

# Pass Data ---------------------------------------------------------------
passdf <- pbp %>% 
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          receiver_gsis_bday = Birth_date,
                          receiver_gsis_pos = Position),
            by = c("receiver_player_id" = "GSIS_ID")) %>%
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          passer_gsis_bday = Birth_date,
                          passer_gsis_pos = Position),
            by = c("passer_player_id" = "GSIS_ID")) %>%
  filter(play_type == "pass",
         sack == 0,
         season >= 2006,
         #two_point_attempt == 0,
         !is.na(air_yards),
         !is.na(receiver_gsis_pos),
         !is.na(passer_gsis_pos)) %>%
  mutate(passer_age = get_age(passer_gsis_bday, game_date, dec = TRUE),
         receiver_age = get_age(receiver_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         wind_speed = case_when(wind >= 15 ~ "high",
                                wind < 15 ~ "low",
                                TRUE ~ "unknown"),
         recFP = 6*pass_touchdown + 2*two_point_converted  + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         passFP =  4*pass_touchdown + 2*two_point_converted  + 0.04*yards_gained - 2*fumble_lost - 2*interception,
         score = ifelse(rush_touchdown == 1 | two_point_conv_result == "success", 1, 0),
         pass_location = ifelse(is.na(pass_location), "unknown", as.character(pass_location)),
         targetline = yardline_100 - air_yards)%>%
  select(game_id, season, week, posteam_type, passer_gsis_pos, passer_age,
         receiver_gsis_pos, receiver_age, two_point_attempt,
         passFP, recFP, yards_gained, pass_touchdown, yardline_100, air_yards,
         pass_location, targetline, shotgun, complete_pass, ydstogo,
         no_huddle, wind_speed, roof, surface, wp)%>%
  mutate_if(is.character, as.factor) %>%
  na.omit()

rm(pbp)

# Train Test Split Data ---------------------------------------------------
passdf_split <- initial_split(passdf, prop = 4/5, strata = season)

passdf_train <- training(passdf_split)
folds <- vfold_cv(passdf_train, 4)
passdf_test <- testing(passdf_split)

#write_parquet(passdf_train,"data/pbp_data/passdf_train.pdata")

# Define Models -----------------------------------------------------------
rec_xgb <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

rec_mars <- mars(
  num_terms = tune(),
  prod_degree = 2,
  prune_method = "exhaustive") %>%
  set_mode("regression") %>%
  set_engine("earth")

rec_rf <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

# Create Helper Functions -------------------------------------------------
create_wf <- function(f,m){
  workflow() %>%
    add_formula(as.formula(f)) %>%
    add_model(m)
}

create_tunes <- function(w){
  print("new tune")
  #doParallel::registerDoParallel()
  start_time <- Sys.time()
  
  x <- w %>%
    tune_grid(resamples = folds,
              grid = 3)
  print(Sys.time() - start_time)
  return(x)
}

final_fit <- function(w,t){
  finalize_workflow(w, select_best(t, "rsq")) %>%
    fit(data=passdf_train)
}

feature_imp <- function(f){
  pull_workflow_fit(f) %>%
    vip(geom = "point")
}

# predict_reception <- function(colnam, funct, data){
#   assign(data, bind_cols(data, colnam = predict(funct, data)), envir = .GlobalEnv)
# }

# Run Tidymodels ----------------------------------------------------------
formula <- "complete_pass ~  season + week + posteam_type + receiver_gsis_pos + receiver_age + passer_gsis_pos + passer_age +
                                   yardline_100 + two_point_attempt +
                                   pass_location + targetline + air_yards +
                                   no_huddle + shotgun + ydstogo +
                                   wind_speed + roof + surface + wp"
models <- tibble(model_name = c("rec_xgb","rec_mars","rec_rf"),
                 model_spec = list(rec_xgb, rec_mars, rec_rf))

# models <- tibble(model_name = c("rec_mars"),
#                  model_spec = list( rec_mars))

df <- expand_grid(models, formula) %>%
  mutate(workflow = map2(formula, model_spec, create_wf),
         tunes = map(workflow, create_tunes),
         metrics = map(tunes, collect_metrics),
         best_fit = map2(workflow, tunes, final_fit),
         feat_imp = map(best_fit, feature_imp),
         predictions = map(best_fit,~predict(.x,new_data = passdf_train)))

df2 <- df %>% 
  select(model_name, predictions) %>% 
  unnest(predictions) %>% 
  pivot_wider(names_from = model_name, values_from = .pred) %>% 
  unnest(col = names(.)) %>%
  bind_cols(passdf_train)

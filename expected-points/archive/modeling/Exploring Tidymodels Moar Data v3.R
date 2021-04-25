# Libraries ----------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(here)
library(arrow)
library(furrr)
library(vip)

library(skimr)

# Raw Data ----------------------------------------------------------------
set.seed(1234)
memory.limit(size=30000)

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

# Rush Data ---------------------------------------------------------------
rushdf <- pbp %>%
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          rusher_gsis_bday = Birth_date,
                          rusher_gsis_pos = Position),
            by = c("rusher_player_id" = "GSIS_ID")) %>%
  filter(play_type == "run",
         #two_point_attempt == 0,
         !grepl("kneel",desc),
         !is.na(yards_gained),
         !is.na(rusher_gsis_pos),
         !is.na(alt_game_id)) %>%
  mutate(rusher_age = get_age(rusher_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         wind_speed = case_when(wind >= 15 ~ "high",
                                wind < 15 ~ "low",
                                TRUE ~ "unknown"),
         rushFP = 6*rush_touchdown  + 2*two_point_converted + 0.1*yards_gained - 2*fumble_lost,
         score = ifelse(rush_touchdown == 1 | two_point_conv_result == "success", 1, 0),
         run_gap = ifelse(is.na(run_gap), "center", as.character(run_gap)),
         run_location = ifelse(is.na(run_location), "unknown", as.character(run_location)),
         run_gap_dir = paste0(run_location,run_gap),
         run_gap_dir = case_when(run_gap_dir %in% c("leftcenter","rightcenter","middleend") | run_location == "unknown" ~ "unknown",
                                 TRUE ~ run_gap_dir)) %>%
  select(season, week, posteam_type, rusher_gsis_pos, rusher_age,
         rushFP, yards_gained, rush_touchdown, yardline_100, two_point_attempt,
         run_gap, run_location, run_gap_dir, qb_scramble,
         no_huddle, shotgun, ydstogo,
         wind_speed, roof, surface, wp) %>%
  mutate_if(is.character, as.factor) %>%
  na.omit()

rm(pbp)


# Train Test Split Data ---------------------------------------------------
rushdf_split <- initial_split(rushdf, prop = 4/5, strata = season)

rushdf_train <- training(rushdf_split)
folds <- vfold_cv(rushdf_train, 4)
rushdf_test <- testing(rushdf_split)


# Define Models -----------------------------------------------------------
rushYds_xgb <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

rushYds_mars <- mars(
  num_terms = tune(),
  prod_degree = 2,
  prune_method = "exhaustive") %>%
  set_mode("regression") %>%
  set_engine("earth")

rushYds_rf <- rand_forest(
  mtry = 4,
  trees = 100,
  min_n = tune()
  ) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")

# Specify Tuning Grids -----------------------------------------------------------
xgd_grid <- grid_regular(
  mtry(range = c(11,22)),
  trees(range = c(250,500)),
  min_n(range = c(20,40)),
  tree_depth(range = c(14,28)),
  #learn_rate(range = c(0.0001,0.001)),
  #loss_reduction(range = c(0.0001,0.001)),
  #sample_size(range = c(0.01,0.05)),
  levels = 2
)

mars_grid <- grid_regular(
  num_terms(range = c(5, 35)),
  levels = 10
)

rf_grid <- grid_regular(
  mtry(range = c(5, 30)),
  #trees(range = c(5,500)),
  #min_n(range = c(50,500)),
  levels = 2
)

# Create Helper Functions -------------------------------------------------
create_wf <- function(f,m){
  workflow() %>%
    add_formula(as.formula(f)) %>%
    add_model(m)
}

create_tunes <- function(w){
  print("new tune")
  doParallel::registerDoParallel()
  start_time <- Sys.time()
  
  x <- w %>%
    tune_grid(resamples = folds,
              grid = 5)
  print(Sys.time() - start_time)
  return(x)
}

final_fit <- function(w,t){
  finalize_workflow(w, select_best(t, "rmse", maximize = FALSE)) %>%
    fit(data=rushdf_train)
}

feature_imp <- function(f){
  pull_workflow_fit(f) %>%
    vip(geom = "point")
}

predict_rushYds <- function(m, f){
  rushdf_train <<- rushdf_train %>%
    bind_cols(predict(f, rushdf_train))
}

predict_rushYds2 <- function(m, f){
  rushdf_test <<- rushdf_test %>%
    bind_cols(predict(f, rushdf_test))
}

# Run Tidymodels ----------------------------------------------------------
formula <- "yards_gained ~  season + week + posteam_type + rusher_gsis_pos + rusher_age +
                                   yardline_100 + two_point_attempt +
                                   run_gap + run_location + run_gap_dir + qb_scramble +
                                   no_huddle + shotgun + ydstogo +
                                   wind_speed + roof + surface + wp"
model_specs <- list(rushYds_xgb, rushYds_mars, rushYds_rf)

df <- expand_grid(formula, model_specs) %>%
  mutate(workflow = map2(formula, model_specs, create_wf),
         tunes = map(workflow, create_tunes),
         metrics = map(tunes, collect_metrics),
         best_fit = map2(workflow, tunes, final_fit),
         feat_imp = map(best_fit, feature_imp),
         predict_rushYds = map2(model_specs, best_fit, predict_rushYds)
  )

df3 <- df %>%
  mutate(model_name = map(model_specs, ~ deparse(substitute(.x))),
         predictions = map(best_fit, ~ predict(.x, new_data =  rushdf_train)))

df4 <- df3 %>%
  unnest_longer(predictions)

# Train Test Split Data ---------------------------------------------------

folds <- vfold_cv(rushdf_train, 4)

df2 <- df2 %>%
  add_row(formula = "yards_gained ~ .pred + .pred1 + .pred2 + rusher_gsis_pos",
          model_specs = list(rushYds_mars),
          workflow = map2(formula, model_specs, create_wf),
          tunes = map(workflow, create_tunes),
          metrics = map(tunes, collect_metrics),
          best_fit = map2(workflow, tunes, final_fit),
          feat_imp = map(best_fit, feature_imp),
          predict_rushYds = map2(model_specs, best_fit, predict_rushYds),
          predict_rushYds2 = map2(model_specs, best_fit, predict_rushYds2))

# df2 <- df2 %>%
#   mutate(actual_rmse = map())


rushdf_train %>%
  rmse(truth = yards_gained, estimate = .pred5)
rushdf_test %>%
  rmse(truth = yards_gained, estimate = .pred5)

# Fit final mars model ----------------------------------------------------

mars_grid <- grid_regular(
  min_n(range = c(100, 20000)),
  levels = 10
)


mars_wf <- create_wf( "yards_gained ~  season + week + posteam_type + rusher_gsis_pos + rusher_age +
                                   yardline_100 + two_point_attempt +
                                   run_gap + run_location + run_gap_dir + qb_scramble +
                                   no_huddle + shotgun + ydstogo +
                                    wind_speed + roof + surface + wp", rushYds_rf)

doParallel::registerDoParallel(cores = 3)
mars_tune <- mars_wf %>%
  tune_grid(resamples = folds,
            grid = mars_grid,
            metrics = metric_set(rmse, rsq, mae))

mars_tune %>%
  collect_metrics() %>%
  filter(.metric == "mae") %>%
  ggplot(aes(min_n, mean)) +
  geom_point()

rushYds_rf <- rand_forest(
  mtry = 4,
  trees = 100,
  min_n = 100) %>%
  set_engine("ranger", importance = "permutation") %>%
  set_mode("regression")


rf_Wf <- create_wf( "yards_gained ~  season + week + posteam_type + rusher_gsis_pos + rusher_age +
                                   yardline_100 + two_point_attempt +
                                   run_gap + run_location + run_gap_dir + qb_scramble +
                                   no_huddle + shotgun + ydstogo +
                                    wind_speed + roof + surface + wp", rushYds_rf)

rf_fit <- rf_Wf %>%
  fit(data=rushdf_train)

rushdf_train$eRushYds2 <- predict(rf_fit, rushdf_train)
rushdf_test$eRushYds2 <- predict(rf_fit, rushdf_test)

rushdf_train %>%
  rsq(truth = yards_gained, estimate = eRushYds$.pred)
rushdf_test %>%
  rsq(truth = yards_gained, estimate = eRushYds$.pred)
rushdf_train %>%
  rsq(truth = yards_gained, estimate = eRushYds2$.pred)
rushdf_test %>%
  rsq(truth = yards_gained, estimate = eRushYds2$.pred)
rushdf_train %>%
  rmse(truth = yards_gained, estimate = eRushYDGam)
rushdf_test %>%
  rmse(truth = yards_gained, estimate = eRushYDGam)


rfplot <- partykit::cforest(yards_gained ~  season + week + posteam_type + rusher_gsis_pos + rusher_age +
          yardline_100 + two_point_attempt +
          run_gap + run_location + run_gap_dir + qb_scramble +
          no_huddle + shotgun + ydstogo +
          wind_speed + roof + surface + wp,
        data=rushdf_train,
        mtry = 4,
        ntree = 100,
        minsplit = 40000)

rushYDModGam <- gam(yards_gained ~ s(yardline_100) + rusher_gsis_pos + shotgun + run_gap_dir + rusher_age + season + qb_scramble,
                    data=rushdf_train, method = "REML", select = TRUE)
rushdf_train$eRushYDGam <- predict(rushYDModGam, rushdf_train)
rushdf_test$eRushYDGam <- predict(rushYDModGam, rushdf_test)

# Prediction Plots --------------------------------------------------------

rushdf_train %>%
  ggplot() +
  geom_density(aes(x = yards_gained), color = "black") +
  geom_density(aes(x = eRushYDGam), color = "blue") +
  #geom_density(aes(x = eRushYds2$.pred), color = "red") +
  #geom_density(aes(x = .pred5), color = "red")+
  xlim(-5,15) +
  facet_wrap(~rusher_gsis_pos)

rushdf_test %>%
  ggplot()+
  #geom_hex(aes(x = yardline_100, y = yards_gained), bins = 25) +
  geom_smooth(aes(yardline_100, yards_gained), color = "black") +
  geom_smooth(aes(yardline_100, .pred2), color = "red") +
  geom_smooth(aes(yardline_100, .pred5), color = "green") +
  ylim(-5,20)+
  facet_wrap(~rusher_gsis_pos)

rushdf_test %>%
  ggplot()+
  #geom_hex(aes(x = wp, y = yards_gained), bins = 25) +
  geom_smooth(aes(wp, .pred2), color = "red") +
  geom_smooth(aes(wp, .pred5), color = "green") +
  geom_smooth(aes(wp, yards_gained), color = "black")+
  ylim(-5,20)+
  facet_wrap(~rusher_gsis_pos)

#research
rushdf %>%
  group_by(round(wp,1)) %>%
  summarise


start_time <- Sys.time()
rf_tunes <- create_tunes(rf_wf)
Sys.time() - start_time

temp <-  map(df$best_fit, ~ predict(.x, new_data =  rushdf_train))

temp2 <- predict(df$best_fit[[1]], folds$splits[[1]][[1]])

folds$splits[[1]][[1]] %>% mutate(xgdpred = predict(df$best_fit[[1]], .))

temp <- rushdf_test %>%
  filter(.pred5 > yardline_100) %>%
  count()

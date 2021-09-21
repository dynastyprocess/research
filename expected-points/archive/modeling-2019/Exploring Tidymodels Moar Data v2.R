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
memory.limit(size=20000)
plan(multisession)

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
         rushFP = 6*rush_touchdown  +2*two_point_converted + 0.1*yards_gained - 2*fumble_lost,
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
folds <- vfold_cv(rushdf_train,4)
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
  mtry = tune(),
  trees = tune(),
  min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode("regression")


# Create Helper Functions -------------------------------------------------
create_wf <- function(f,m){
  workflow() %>%
    add_formula(as.formula(f)) %>%
    add_model(m)
}


# Run Tidymodels ----------------------------------------------------------
formula <- "yards_gained ~  season + week + posteam_type + rusher_gsis_pos + rusher_age +
                                   yardline_100 + two_point_attempt +
                                   run_gap + run_location + run_gap_dir + qb_scramble +
                                   no_huddle + shotgun + ydstogo +
                                   wind_speed + roof + surface + wp"
model_specs <- list(rushYds_xgb, rushYds_mars, rushYds_rf)

df <- expand_grid(formula, model_specs) %>%
  mutate(workflow = map2(formula, model_specs, create_wf)
  )
         
boost_wf <- workflow() %>%
  add_formula(yards_gained ~ yardline_100 + rusher_gsis_pos + shotgun + run_gap_dir + season + rusher_age) %>%
  add_model(boost_model)

doParallel::registerDoParallel()

boost_metrics <- boost_wf %>%
  tune_grid(resamples = folds,
            grid = 10)

boost_metrics %>%
  collect_metrics() %>%
  filter(.metric == "rsq", mean > 0.02) %>%
  select(mean, min_n, mtry, trees, tree_depth, learn_rate, loss_reduction, sample_size) %>%
  pivot_longer(min_n:sample_size,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE, size = 4) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x=NULL, y = "rsq")

boost_grid <- grid_regular(
  mtry(range = c(11,22)),
  trees(range = c(250,500)),
  min_n(range = c(20,40)),
  tree_depth(range = c(14,28)),
  #learn_rate(range = c(0.0001,0.001)),
  #loss_reduction(range = c(0.0001,0.001)),
  #sample_size(range = c(0.01,0.05)),
  levels = 2
)

boost_tune <- boost_wf %>%
  tune_grid(resamples = folds,
            grid = boost_grid) 

final_boost <- finalize_workflow(
  boost_wf,
  select_best(boost_tune, "rsq")
)

final_boost_fit <- final_boost %>%
  fit(data=rushdf_train)

# boost_tree2 <- boost_wf %>%
#   fit(data=rushdf_train)

vip(pull_workflow_fit(boost_tree2))

#Rushing Yards Model
rushYDModGam <- gam(yards_gained ~ s(yardline_100) + rusher_gsis_pos + shotgun + run_gap_dir  + as.factor(season) + s(rusher_age),
                    data=rushdf_train, method = "REML", select = TRUE)
rushdf$eRushYDGam <- predict(rushYDModGam, rushdf)
rushdf$eRushYDGam = ifelse(rushdf$eRushYDGam > rushdf$yardline_100,rushdf$yardline_100,rushdf$eRushYDGam)

#MARS

mars_wf <- workflow() %>%
  add_formula(yards_gained ~ yardline_100 + rusher_gsis_pos + shotgun + run_gap_dir + season + rusher_age + ydstogo) %>%
  add_model(mars_mod)

doParallel::registerDoParallel()

mars_grid <- grid_regular(
  num_terms(range = c(6, 11)),
  levels = 5
)

mars_tune <- mars_wf %>%
  tune_grid(resamples = folds,
            grid = mars_grid)

final_mars <- finalize_workflow(
  mars_wf,
  select_best(mars_tune, "rsq")
)

final_mars_fit <- final_mars %>%
  fit(data=rushdf_train)

final_mars %>%
  fit(data=rushdf_train
  ) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

temp <- final_mars %>%
  fit(rushdf_train) %>%
  pull_workflow_fit() %>%
  vi() %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)

# mars_tune %>%
#   collect_metrics() %>%
#   filter(.metric == "rsq") %>%
#   select(prune_method, mean, num_terms, prod_degree) %>%
#   pivot_longer(num_terms:prod_degree,
#                values_to = "value",
#                names_to = "parameter") %>%
#   ggplot(aes(value, mean, color = prune_method)) +
#   geom_point(size = 4) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x=NULL, y = "rsq")

#Rand Forest


rf_wf <- workflow() %>%
  add_formula(yards_gained ~ yardline_100 + rusher_gsis_pos + shotgun + run_gap_dir + season + rusher_age + ydstogo) %>%
  add_model(rf_model)

rf_grid <- grid_regular(
  mtry(range = c(5, 30)),
  trees(range = c(5,500)),
  min_n(range = c(50,500)),
  levels = 5
)

rf_tune <- rf_wf %>%
  tune_grid(resamples = folds,
            grid = rf_grid)

rf_tunes %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE, size = 4) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x=NULL, y = "rmse")

final_rf <- finalize_workflow(
  rf_wf,
  select_best(rf_tune, "rsq")
)

final_rf_fit <- final_rf %>%
  fit(data=rushdf_train)


# Fitting Rushing Yards Models --------------------------------------------
rushdf_train <- rushdf_train %>%
  bind_cols(predict(final_rf_fit, rushdf_train),
        predict(final_mars_fit, rushdf_train))

rushdf_train <- rushdf_train %>%
  mutate(xgbpredRushYds = predict(boost_tree2, rushdf_train),
         marspredRushYds = predict(final_mars_fit, rushdf_train))

rushdf_split2 <- initial_split(rushdf_train, prop = 3/4)

rushdf_train2 <- training(rushdf_split2)
folds2 <- vfold_cv(rushdf_train2,4)
rushdf_test2 <- testing(rushdf_split2)

combo_mod <- mars(
  num_terms = tune(),
  prod_degree = 2,
  prune_method = "exhaustive") %>%
  set_mode("regression") %>%
  set_engine("earth")

combo_wf <- workflow() %>%
  add_formula(yards_gained ~ .pred5 + .pred6 + .pred3 + yardline_100) %>%
  add_model(combo_mod)

doParallel::registerDoParallel()

combo_tune <- combo_wf %>%
  tune_grid(resamples = folds2,
            grid = 5)

final_mars <- finalize_workflow(
  combo_wf,
  select_best(combo_tune, "rsq")
)
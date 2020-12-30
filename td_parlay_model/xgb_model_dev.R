suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(slider)
  library(lubridate)
  
  # Plotting
  library(ggbeeswarm)
  library(ggthemes)
  library(directlabels)
  
  #EDA
  library(skimr)
  library(usemodels)
  library(tidymodels)
  library(vip)
  library(baguette)
  
  library(mgcv)
  
})

# Import Data -------------------------------------------------------------
setwd(here::here())

memory.size(60000)

ep_feature_mart <- read_arrow("C:/Users/syd23/Documents/DynastyProcess/db/feature_mart.pdata")

td_model_prep <- ep_feature_mart %>%
  filter(as.character.numeric_version(Season) >= 2006, as.character.numeric_version(Season) < 2020) %>%
  select(-(contains("ecr") & !(contains("combo") | contains("teammate")))) %>% 
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id) %>%
  mutate(across(.cols = c(parlay_td, posteam_type, spread_line, total_line, implied_total,
                          ecr_ovr_combo, ecr_pos_combo,
                          teammate_ecr_rank, teammate_ecr_gap_to_better,
                          teammate_ecr_gap_to_next, teammate_ecr_gap_to_best),
                .fns = ~lead(.x), 
                .names = "{.col}_next"),
         across(contains("teammate"), ~replace_na(.x, 0)),
         across(contains("teammate"), ~round(.x, 2)),
         Week = as.numeric(Week),
         week_group = as.factor(case_when(Week > 17 ~ "Playoffs",
                                          TRUE ~ as.character(Week)))) %>% 
  ungroup()

# td_model_prep %>% filter(is.na(parlay_td_next), Season == 2020)
# names(which(colSums(is.na(td_model_prep))>0))
# names(td_model_prep %>% select( )

# Model functions ---------------------------------------------------------
get_split <- function(df){
  set.seed(1234)
  initial_split(df, prop = 4/5, strata = Season)
}

get_split_data <- function(df, split_type){
  set.seed(1234)
  if (split_type == "train")
  {training(df)}
  else {testing(df)}
}

add_pred <- function(df, mod){
  df %>%
    bind_cols(predict(mod, .)) %>%
    rename(parlay_td_next_pred = .pred)
}

get_metrics <- function(df, mod){
  df %>% 
    bind_cols(predict(mod, .)) %>%
    rmse(parlay_td_next, .pred)
}

# Tidymodels --------------------------------------------------------------
# get_wf <- function(df){
#   log_spec <- bag_mars(
#     num_terms = 16,
#     prod_degree = 2) %>%
#     set_engine("earth", times = 2) %>% 
#     #set_engine("earth", nfold = 5, ncross = 10, pmethod = "cv", thresh = 0.001) %>%
#     set_mode("regression")
# 
#   model_rec <- recipe(parlay_td_next ~ ., data = df) %>%
#     update_role(week_season, Week, Team, gsis_game_id, Name, gsis_id, sportradar_id, new_role = "id") %>%
#     #step_log(contains("ecr") & !contains("rank")) %>% 
#     step_dummy(c(Season, week_group, posteam_type, posteam_type_next), one_hot = TRUE) %>%
#     step_zv(all_predictors()) %>%
#     step_normalize(all_predictors())
#   
#   workflow() %>%
#     add_model(log_spec) %>%
#     add_recipe(model_rec)
#   
# }

get_wf <- function(df){
  xgb_spec <- boost_tree(
    trees = tune(), 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune(),                         ## step size
  ) %>% 
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  model_rec <- recipe(parlay_td_next ~ ., data = df) %>%
    update_role(week_season, Week, Team, gsis_game_id, Name, gsis_id, sportradar_id, new_role = "id") %>%
    #step_log(contains("ecr") & !contains("rank")) %>% 
    step_dummy(c(Season, week_group, posteam_type, posteam_type_next), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_predictors())
  
  workflow() %>%
    add_model(xgb_spec) %>%
    add_recipe(model_rec)
  
}

get_fit <- function(wf,df){
  
  xgb_grid <- grid_latin_hypercube(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), df),
    learn_rate(),
    size = 5
  )
  
  folds <- vfold_cv(df, 4)
  
  log_res <- tune_grid(
    wf,
    resamples = folds,
    grid = xgb_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, parallel_over = "everything")
  )
  
  print(log_res %>%
    collect_metrics() %>%
    filter(.metric == "rmse") %>%
    select(mean, trees:sample_size) %>%
    pivot_longer(trees:sample_size,
                 values_to = "value",
                 names_to = "parameter"
    ) %>%
    ggplot(aes(value, mean, color = parameter)) +
    geom_point(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "rmse"))
  
  best_log <- select_best(log_res, "rmse")
  
  final_log <- finalize_workflow(
    wf,
    best_log
  )
  
  fit(final_log, data = df)
  
}

# get_fit <- function(wf,df){
# 
#   log_grid <- expand_grid(num_terms = seq(4,18,2),
#                           prod_degree = c(1,2))
#   #tibble(num_terms = seq(5,25,5))
# 
#   folds <- vfold_cv(df, 2)
# 
#   log_res <- tune_grid(
#     wf,
#     resamples = folds,
#     grid = log_grid,
#     metrics = metric_set(rmse),
#     control = control_grid(save_pred = TRUE, event_level = "second")
#   )
# 
#   best_log <- select_best(log_res, "rmse")
# 
#   final_log <- finalize_workflow(
#     wf,
#     best_log
#   )
# 
#   fit(wf, data = df)
# 
# }

# Model data ----------------------------------------------------------------
# new_DF <- ep_model_data[rowSums(is.na(ep_model_data)) > 0,]
# names(which(colSums(is.na(ep_model_data))>0))
# names(ep_model_data %>% select(!where(is.numeric)))
# library(furrr)
# no_cores <- availableCores() - 2
# plan(multisession, workers = no_cores, gc = T)

ep_model_data <- td_model_prep %>%
  #mutate(parlay_td_next = as.factor(parlay_td_next)) %>% 
  filter(!is.na(parlay_td_next), Pos == "WR") %>%
  group_by(Pos) %>% 
  nest() %>%
  ungroup() %>% 
  mutate(df_split = map(data, ~get_split(.x)),
         df_train = map(df_split, ~get_split_data(.x, "train")),
         df_test = map(df_split, ~get_split_data(.x, "test")))

ep_model_fits <- ep_model_data %>%
  mutate(pos_wf = map(df_train, ~get_wf(.x)),
         model_fit = map2(pos_wf, df_train, ~get_fit(.x,.y)),
         df_test_predict = map2(df_test, model_fit, add_pred),
         df_test_metrics = map2(df_test, model_fit, get_metrics))


#analyze ECR fits
temp <- add_pred(ep_model_fits$df_test[[1]], ep_model_fits$model_fit[[1]])

temp %>% rmse(as.numeric(parlay_td_next), as.numeric(.pred_1))


wr_td_pred <- glm(parlay_td_next ~ ecr_pos_combo + ecr_ovr_combo + ecr_pos_combo_next + ecr_ovr_combo_next,
          data = ep_model_data$df_train[[1]],
          family = binomial)

wr_predictions <- prep_2020 %>%
  filter(Pos == "WR") %>% 
  mutate(.pred = predict(wr_td_pred, ., type = "response"))

cleanModel <- function(mod){
  mod$fit$fit$fit$bx <- NULL
  
  mod$fit$fit$fit$glm.list <- NULL
  mod$fit$fit$fit$call <- NULL
  mod$fit$fit$spec$eng_args <- NULL
  mod$fit$fit$spec$method$fit$args <- NULL
  mod
}

qb_td_pred <- ep_model_fits %>% filter(Pos == "QB") %>% pull(model_fit) %>% .[[1]] %>% cleanModel()
rb_td_pred <- ep_model_fits %>% filter(Pos == "RB") %>% pull(model_fit) %>% .[[1]] %>% cleanModel()
wr_td_pred <- ep_model_fits %>% filter(Pos == "WR") %>% pull(model_fit) %>% .[[1]] %>% cleanModel()
te_td_pred <- ep_model_fits %>% filter(Pos == "TE") %>% pull(model_fit) %>% .[[1]] %>% cleanModel()


cleanWF <- function(mod){
  mod$pre$actions$recipe$recipe$steps <- NULL
  mod$fit$actions$model$spec$args <- NULL
  mod
}
wr_td_wf <- ep_model_fits %>% filter(Pos == "WR") %>% pull(pos_wf) %>% .[[1]]

summary(wr_td_pred$fit$fit$fit)
summary(wr_td_pred$fit$fit$fit$imp)

ep_model_fits$Pos
ep_model_fits$df_test_metrics

save(wr_td_pred, file = "parlay_pred_models_updated.rda")

# Metrics -----------------------------------------------------------------
plot(wr_td_pred$fit$fit$fit$thresh)

wr_td_pred %>%
  pull_workflow_fit() %>%
  vip(geom = "point", num_features = 10)

pdp::partial(ep_model_fits$model_fit[[1]]$fit$fit$fit,
             pred.var = "total_fp_share_roll10",
             train = ep_model_fits$model_fit[[1]]$pre$mold$predictors) %>%
  autoplot()

pdp::partial(ep_model_fits$model_fit[[1]]$fit$fit$fit,
             pred.var = c("ecr_pos_combo","ecr_pos_combo_next"),
             train = ep_model_fits$model_fit[[1]]$pre$mold$predictors) %>%
  autoplot()


temp <- ep_model_data$df_train[[2]] %>%
  bind_cols(predict(rb_td_pred, ., type = "prob")) %>%
  rename(parlay_td_next_pred = .pred_1)

temp %>% 
  ggplot(aes(x=parlay_td_next_pred)) +
  geom_histogram()

temp %>% 
  summarise(min(parlay_td_next_pred),
            max(parlay_td_next_pred))
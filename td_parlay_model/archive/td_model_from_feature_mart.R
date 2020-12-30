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

ep_feature_mart <- read_arrow("C:/Users/syd23/Documents/DynastyProcess/db/feature_mart.pdata")

td_model_prep <- ep_feature_mart %>%
  select(-(contains("ecr") & !(contains("combo") | contains("teammate")))) %>% 
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id) %>%
  mutate(across(.cols = c(parlay_td, posteam_type, spread_line, total_line, implied_total,
                          ecr_ovr_combo, ecr_pos_combo,
                          teammate_ecr_rank, teammate_ecr_gap_to_better,
                          teammate_ecr_gap_to_next, teammate_ecr_gap_to_best),
                .fns = ~lead(.x), 
                .names = "{.col}_next")) %>% 
  ungroup()

# td_model_prep %>% filter(is.na(parlay_td_next), Season == 2020)
# names(which(colSums(is.na(td_model_prep))>0))
# names(td_model_prep %>% select( )

# Model functions ---------------------------------------------------------
get_split <- function(df){
  initial_split(df, prop = 4/5, strata = Season)
}

get_split_data <- function(df, split_type){
  if (split_type == "train")
  {training(df)}
  else {testing(df)}
}

get_fit <- function(df){
  fit(earth_workflow, data=df)
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
get_wf <- function(df){
  set.seed(1234)

  log_spec <- mars(
    num_terms = tune(),
    prod_degree =  tune()) %>%
    set_engine("earth", varmod.method = "gam", nfold = 3, ncross = 3, pmethod = "cv") %>%
    set_mode("regression")

  model_rec <- recipe(parlay_td_next ~ ., data = df) %>%
    update_role(week_season, Team, gsis_game_id, Name, gsis_id, sportradar_id, new_role = "id") %>%
    #step_corr(all_numeric()) %>%
    step_dummy(c(Season, Week, posteam_type, posteam_type_next), one_hot = TRUE) %>%
    step_zv(all_predictors()) %>% 
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors(), threshold = 0.9)
  
  log_wf <- workflow() %>%
    add_model(log_spec) %>%
    add_recipe(model_rec)
}

get_parlay_fit <- function(df, wf){
  
  log_grid <- expand_grid(num_terms = seq(2,14,2),
                          prod_degree = c(1,2))
  #tibble(num_terms = seq(5,25,5))
  
  folds <- vfold_cv(df, 3, strata = Season)
  
  log_res <- tune_grid(
    wf,
    resamples = folds,
    grid = log_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, event_level = "second")
  )
  
  best_log <- select_best(log_res, "rmse")
  
  final_log <- finalize_workflow(
    wf,
    best_log
  )
  
  fit(final_log, data = df)
  
}

# Model data ----------------------------------------------------------------
# new_DF <- ep_model_data[rowSums(is.na(ep_model_data)) > 0,]
# names(which(colSums(is.na(ep_model_data))>0))

ep_model_data <- td_model_prep %>%
  filter(as.character.numeric_version(Season) < 2020, !is.na(parlay_td_next), Pos == "WR") %>%
  group_by(Pos) %>% 
  nest() %>%
  ungroup() %>% 
  mutate(df_split = map(data, ~get_split(.x)),
         df_train = map(df_split, ~get_split_data(.x, "train")),
         df_test = map(df_split, ~get_split_data(.x, "test")),
         pos_wf = map(df_train, ~get_wf(.x)))

ep_model_fits <- ep_model_data %>%
  mutate(model_fit = map2(df_train, pos_wf, ~get_parlay_fit(.x,.y)))
         # df_test_predict = map2(df_test, model_fit, add_pred),
         # df_test_metrics = map2(df_test, model_fit, get_metrics))

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


ep_model_fits$Pos
ep_model_fits$df_test_metrics

save(wr_td_pred, wr_td_wf, file = "parlay_pred_models_updated.rda")

# Metrics -----------------------------------------------------------------

wr_td_pred %>%
  pull_workflow_fit() %>%
  vip(geom = "point", num_features = 15)


summary(rb_td_pred$fit$fit$fit)


temp <- ep_model_data$df_train[[2]] %>%
  bind_cols(predict(rb_td_pred, ., type = "prob")) %>%
  rename(parlay_td_next_pred = .pred_1)

temp %>% 
  ggplot(aes(x=parlay_td_next_pred)) +
  geom_histogram()

temp %>% 
  summarise(min(parlay_td_next_pred),
            max(parlay_td_next_pred))


# Test recipe -------------------------------------------------------------
model_rec <- pull_workflow_preprocessor(wr_td_wf)

pca_prep <- prep(model_rec)

tidied_pca <- tidy(pca_prep, 5)

rb_clusters_wide <- juice(pca_prep) %>%
  pivot_longer(cols = starts_with("PC"), names_to = "Cluster")

tidied_pca %>%
  filter(component %in% paste0("PC", c(004,001,006))) %>%
  group_by(component) %>%
  top_n(30, abs(value)) %>%
  ungroup() %>%
  mutate(terms = tidytext::reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  labs(
    title = "Variable Importance by Tier",
    y = NULL,
    fill = "Positive Contribution?"
  )

  
sdev <- pca_prep$steps[[5]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

temp <- tibble(
  component = unique(tidied_pca$component),
  percent_var = percent_variation, ## use cumsum() to find cumulative, if you prefer
  cum_Var = cumsum(percent_variation)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_var)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "Percent variance explained by each PCA component")
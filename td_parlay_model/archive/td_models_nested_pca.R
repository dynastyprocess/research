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
})

# Import Data -------------------------------------------------------------
setwd(here::here())

con <- DBI::dbConnect(odbc::odbc(), "dynastyprocess")

ep <- dbGetQuery(con, "SELECT * FROM dp_expectedpoints WHERE Pos in ('QB','RB','WR','TE')")

ecr_archive_pos <- dbGetQuery(con, "SELECT week, year, sportsdata_id, page_pos as pos, ecr as ecr_ovr from fp_ecr_archive
                                where page_type in ('weekly-qb','weekly-rb','weekly-wr','weekly-te') and sportsdata_id is not null")

ecr_archive_ovr <- dbGetQuery(con, "SELECT week, year, sportsdata_id, pos, ecr as ecr_pos from fp_ecr_archive
                                where page_type = 'weekly-op' and sportsdata_id is not null")

pbp <- dbGetQuery(con, "SELECT distinct game_id, posteam, posteam_type, total_line,
                               case when posteam_type = 'away' then spread_line else -spread_line end as 'spread_line'
                        FROM nflfastr_pbp where posteam is not null and posteam <> ''") %>% 
  mutate(posteam = case_when(posteam == "ARZ" ~ "ARI",
                   posteam == "BLT" ~ "BAL",
                   posteam == "CLV" ~ "CLE",
                   posteam == "HST" ~ "HOU",
                   posteam == "JAC" ~ "JAX",
                   posteam == "LA" ~ "LAR",
                   posteam == "STL" ~ "LAR",
                   posteam == "SAN" ~ "LAC",
                   posteam == "SD" ~ "LAC",
                   posteam == "SL" ~ "LAR",
                   TRUE ~ posteam))

dbDisconnect(con)
rm(con)

# Functions -------------------------------------------------------------
get_rate <- function(x,y){
  rate <- sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE)
  
  ifelse(is.nan(rate) | is.infinite(rate), 0, rate)
}


# Rolling averages --------------------------------------------------------
ep_lagged_lines <- ep %>%
  #filter(Name == "Christian Kirk") %>% 
  filter(Season >= 2004) %>% 
  inner_join(pbp, by = c("gsis_game_id" = "game_id", "Team" = "posteam")) %>% 
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id, Pos) %>%
  filter(max(Season) == 2020) %>% #Take out to train model
  mutate(game_number = row_number(),
         implied_total = if_else(spread_line<=0, (total_line+spread_line)/2 - spread_line, (total_line-spread_line)/2),
         parlay_td = if_else(rush_td + rec_td > 0,1,0),
         across(.cols = c(posteam_type, spread_line, total_line, parlay_td, implied_total),
                .fns = ~lead(.x),
                .names = "{.col}_next"),
         across(.cols = where(is.numeric) & !contains("next"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 15, .after = 0),
                .names = "{.col}_roll16"),
         across(.cols = where(is.numeric) & !contains("next") & !contains("roll"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 7, .after = 0),
                .names = "{.col}_roll8"),
         across(.cols = where(is.numeric) & !contains("next") & !contains("roll"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 3, .after = 0),
                .names = "{.col}_roll4"),
         across(.cols = where(is.numeric) & !contains("next") & !contains("roll"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 2, .after = 0),
                .names = "{.col}_roll3"),         
         across(.cols = where(is.numeric) & !contains("next") & !contains("roll"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 1, .after = 0),
                .names = "{.col}_roll2"),         
         across(.cols = where(is.numeric) & !contains("next") & !contains("roll"),
                .fns = ~lag(.x),
                .names = "{.col}_roll1"),
         
         racr_roll16 = slide2_dbl(rec_yd, rec_ay, ~get_rate(.x,.y), .before = 15),
         rec_tar_share_roll16 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 15),
         rec_ay_share_roll16 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 15),
         rec_wopr_roll16 = 1.5*rec_tar_share_roll16 + 0.7*rec_ay_share_roll16,
         ypt_roll16 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 15),
         rec_comp_rate_roll16 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 15),
         rec_td_rate_roll16 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 15),
         ypc_roll16 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 15),
         rush_td_rate_roll16 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 15),
         
         racr_roll8 = slide2_dbl(rec_yd, rec_ay, ~get_rate(.x,.y), .before = 7),
         rec_tar_share_roll8 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 7),
         rec_ay_share_roll8 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 7),
         rec_wopr_roll8 = 1.5*rec_tar_share_roll8 + 0.7*rec_ay_share_roll8,
         ypt_roll8 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 7),
         rec_comp_rate_roll8 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 7),
         rec_td_rate_roll8 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 7),
         ypc_roll8 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 7),
         rush_td_rate_roll8 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 7),
         
         racr_roll4 = slide2_dbl(rec_yd, rec_ay, ~get_rate(.x,.y), .before = 3),
         rec_tar_share_roll4 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 3),
         rec_ay_share_roll4 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 3),
         rec_wopr_roll4 = 1.5*rec_tar_share_roll4 + 0.7*rec_ay_share_roll4,
         ypt_roll4 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 3),
         rec_comp_rate_roll4 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 3),
         rec_td_rate_roll4 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 3),
         ypc_roll4 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 3),
         rush_td_rate_roll4 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 3),

         racr_roll3 = slide2_dbl(rec_yd, rec_ay, ~get_rate(.x,.y), .before = 2),
         rec_tar_share_roll3 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 2),
         rec_ay_share_roll3 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 2),
         rec_wopr_roll3 = 1.5*rec_tar_share_roll3 + 0.7*rec_ay_share_roll3,
         ypt_roll3 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 2),
         rec_comp_rate_roll3 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 2),
         rec_td_rate_roll3 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 2),
         ypc_roll3 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 2),
         rush_td_rate_roll3 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 2),
         
         racr_roll2 = slide2_dbl(rec_yd, rec_ay, ~get_rate(.x,.y), .before = 1),
         rec_tar_share_roll2 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 1),
         rec_ay_share_roll2 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 1),
         rec_wopr_roll2 = 1.5*rec_tar_share_roll2 + 0.7*rec_ay_share_roll2,
         ypt_roll2 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 1),
         rec_comp_rate_roll2 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 1),
         rec_td_rate_roll2 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 1),
         ypc_roll2 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 1),
         rush_td_rate_roll2 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 1),
         
         racr_roll1 = lag(rec_yd)/ lag(rec_ay),
         rec_tar_share_roll1 = lag(rec_tar)/ lag(pass_att_team),
         rec_ay_share_roll1 = lag(rec_ay)/ lag(pass_ay_team),
         rec_wopr_roll1 = 1.5*rec_tar_share_roll1 + 0.7*rec_ay_share_roll1,
         ypt_roll1 = lag(rec_yd)/lag(rec_tar),
         rec_comp_rate_roll1 = lag(rec_comp)/lag(rec_tar),
         rec_td_rate_roll1 = lag(rec_td)/lag(rec_comp),
         ypc_roll1 = lag(rush_yd)/lag(rush_att),
         rush_td_rate_roll1 = lag(rush_td)/lag(rush_att),
         across(.cols = contains("roll1"),
                .fn =  ~ifelse(is.nan(.x) | is.infinite(.x) | is.na(.x), 0, .x))) %>% 
  ungroup() %>% 
  left_join(ecr_archive_ovr, by = c("Season" = "year", "Week" = "week", "sportradar_id" = "sportsdata_id", "Pos" = "pos")) %>% 
  left_join(ecr_archive_pos, by = c("Season" = "year", "Week" = "week", "sportradar_id" = "sportsdata_id", "Pos" = "pos")) %>%
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id, Pos) %>%
  mutate(ecr_pos = as.numeric(ecr_pos),
         ecr_ovr = as.numeric(ecr_ovr),
         across(where(is.numeric), round, 2)) %>%
  ungroup() %>% 
  select(Season, Week, week_season, Team, gsis_game_id, Name, Pos, gsis_id, sportradar_id, player_age, game_number,
         posteam_type_next, spread_line_next, total_line_next, parlay_td_next, implied_total_next, where(is.numeric),
         -contains("Season_"), -contains("Week_"), -contains("week_season_num_"), -contains("player_age_"),
         -contains("game_number_"), -contains("next_roll"))

write_arrow(ep_lagged_lines, "model_roll.pdata")
ep_lagged_lines <- read_arrow("model_roll.pdata")

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

get_ecr_predictions <- function(df){
  earth_spec <- mars(prod_degree = 2) %>%
    set_engine("earth") %>%
    set_mode("regression")
  
  #ecr_ovr
  ecr_ovr_df <- df %>% 
    select(-c(Week, Team, gsis_game_id, Name, gsis_id, sportradar_id, ecr_pos, contains("next"))) %>%
    filter(!is.na(ecr_ovr))
  
  ecr_ovr_fit <- earth_spec %>% 
    fit(ecr_ovr ~ ., data = ecr_ovr_df)
  
  #ecr_pos
  ecr_pos_df <- df %>% 
    select(-c(Week, Team, gsis_game_id, Name, gsis_id, sportradar_id, ecr_ovr, contains("next"))) %>% 
    filter(!is.na(ecr_pos))
  
  ecr_pos_fit <- earth_spec %>% 
    fit(ecr_pos ~ ., data = ecr_pos_df)
  
  df %>%
    bind_cols(predict(ecr_ovr_fit, .)) %>% 
    rename(ecr_ovr_pred=.pred) %>% 
    bind_cols(predict(ecr_pos_fit, .)) %>% 
    rename(ecr_pos_pred=.pred) %>%
    arrange(gsis_id, gsis_game_id) %>%
    group_by(gsis_id) %>% 
    mutate(ecr_ovr_pred = ifelse(is.na(ecr_ovr),ecr_ovr_pred,ecr_ovr),
           ecr_pos_pred = ifelse(is.na(ecr_pos),ecr_pos_pred,ecr_pos),
           across(.cols = c(ecr_ovr_pred, ecr_pos_pred),
                  .fns = ~lead(as.numeric(.x)),
                  .names = "{.col}_next")) %>% 
    ungroup() %>% 
    select(-c(Week, Team, gsis_game_id, Name, gsis_id, sportradar_id, ecr_ovr, ecr_pos, ecr_ovr_pred, ecr_pos_pred)) %>% 
    filter(!is.na(ecr_ovr_pred_next), !is.na(ecr_pos_pred_next))
}


# Tidymodels --------------------------------------------------------------
library(mgcv)
get_parlay_fit <- function(df){
  set.seed(1234)
  print("Training MARS")
  
  log_spec <- mars(
    num_terms = tune(),
    prod_degree =  tune()#,
    #prune_method = "cv"
    ) %>%
    set_engine("earth", varmod.method = "gam", nfold = 3, ncross = 3, pmethod = "cv") %>%
    set_mode("regression")
  
  log_grid <- #tibble(num_terms = seq(5,25,5))
              expand_grid(num_terms = seq(2,14,2),
                          prod_degree = c(1,2,3))

  model_rec <- recipe(parlay_td_next ~ ., data = df) %>% 
    step_dummy(posteam_type_next) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(all_predictors()) %>%
    step_pca(all_predictors(), threshold = 0.9)
  
  log_wf <- workflow() %>%
    add_model(log_spec) %>%
    add_recipe(model_rec)
  
  folds <- vfold_cv(df, 4, strata = Season)
  
  log_res <- tune_grid(
    log_wf,
    resamples = folds,
    grid = log_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE, event_level = "second")
  )
  
  best_log <- select_best(log_res, "rmse")
  
  final_log <- finalize_workflow(
    log_wf,
    best_log
  )
  
  fit(final_log, data = df)
  
}

# Model data ----------------------------------------------------------------
library(furrr)
no_cores <- availableCores() - 2
plan(multisession, workers = no_cores, gc = T)

ep_model_data <- ep_lagged_lines %>%
  filter(Season >= 2006, Season < 2020) %>%
  #mutate(parlay_td_next = as.factor(parlay_td_next)) %>% 
  group_by(Pos) %>% 
  nest() %>%
  ungroup() %>% 
  mutate(df_ecr = future_map(data, ~get_ecr_predictions(.x), .progress = TRUE),
         df_split = map(df_ecr, ~get_split(.x)),
         df_train = map(df_split, ~get_split_data(.x, "train")),
         df_test = map(df_split, ~get_split_data(.x, "test")))

ep_model_fits <- ep_model_data %>% 
  #filter(Pos == "RB") %>% 
  mutate(model_fit = map(df_train, ~get_parlay_fit(.x), .progress = TRUE),
         df_test_predict = map2(df_test, model_fit, add_pred),
         df_test_metrics = map2(df_test, model_fit, get_metrics))

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

ep_model_fits$Pos
ep_model_fits$df_test_metrics

save(qb_td_pred,rb_td_pred,wr_td_pred,te_td_pred, file = "parlay_pred_models.rda")

# Metrics -----------------------------------------------------------------

rb_td_pred %>%
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
rb_train <- ep_model_data$df_train[[3]]

model_rec <- recipe(parlay_td_next ~ ., data = rb_train) %>% 
  step_dummy(posteam_type_next) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), threshold = 0.9)

pca_prep <- prep(model_rec)

tidied_pca <- tidy(pca_prep, 4)

rb_clusters_wide <- juice(pca_prep) %>%
  pivot_longer(cols = starts_with("PC"), names_to = "Cluster")

tidied_pca %>%
  filter(component %in% paste0("PC", c(01,02,03,15))) %>%
  group_by(component) %>%
  top_n(15, abs(value)) %>%
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
suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(slider)
  library(lubridate)
  #library(glue)
  #library(magrittr)
  
  # Plotting
  library(ggbeeswarm)
  library(ggthemes)
  library(directlabels)
  #library(ggimage)
  #library(grid)
  #library(ggrepel)
  #library(nflfastR)
  
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

ecr <- dbGetQuery(con, "SELECT * from fp_ecr where page_type in ('weekly-offense','weekly-qb','weekly-rb','weekly-wr','weekly-te')")

# palyerid <- dbGetQuery(con, "select * from dp_playerids")
# ecr_sum <- ecr %>%
#   group_by(fp_page, page_type, ecr_type) %>%
#   summarise(n(),
#             min(scrape_date),
#             max(scrape_date))
# 
# ecr_sum <- ecr_archive %>%
#   group_by(page_type, ecr_type) %>%
#   summarise(n())
# 
# ecr_archive_sum <- ecr_archive_ovr %>%
#   group_by(week, year, sportsdata_id, page_pos) %>%
#   summarise(n())

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

games <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")

games_away <- games %>%
  select(game_id, week, season, gameday, posteam = away_team, spread_line, total_line, result) %>% 
  mutate(posteam_type = "away",
         implied_total = if_else(spread_line<=0, (total_line+spread_line)/2 - spread_line, (total_line-spread_line)/2))

games_home <- games %>%
  select(game_id, week, season, gameday, posteam = home_team, spread_line, total_line, result) %>% 
  mutate(posteam_type = "home",
         spread_line = -spread_line,
         implied_total = if_else(spread_line<=0, (total_line+spread_line)/2 - spread_line, (total_line-spread_line)/2))

games_combined <- 
  bind_rows(games_away, games_home) %>% 
  arrange(game_id) %>% 
  group_by(posteam) %>% 
  mutate(next_spread_line = lead(spread_line),
         next_total_line = lead(total_line),
         next_implied_total = lead(implied_total),
         last_gameday = max(if_else(!is.na(result),gameday,as_date('1990-1-1')))) %>% 
  ungroup() %>% 
  filter(gameday == last_gameday, season == year(today())) %>% 
  select(game_id, next_spread_line, next_total_line, next_implied_total)


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
  mutate(across(.cols = c(ecr_pos, ecr_ovr),
                .fns = ~lead(as.numeric(.x)),
                .names = "{.col}_next"),
         across(where(is.numeric), round, 2)) %>%
  select(Season, Week, week_season, Team, gsis_game_id, Name, Pos, gsis_id, player_age, game_number,
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
    bind_cols(predict(mod, ., type = "prob")) %>%
    rename(parlay_td_next_pred = .pred_1)
}

get_metrics <- function(df){
  df %>% 
    metrics(parlay_td_next, parlay_td_next_pred)
}

get_ecr_predictions <- function(df, pos){
  earth_spec <- mars(prod_degree = 2, prune_method = "exhaustive") %>%
    set_engine("earth") %>%
    set_mode("regression")
  
  #ecr_ovr
  ecr_ovr_df <- df %>% 
    select(-c(ecr_pos_next, parlay_td_next)) %>% 
    filter(!is.na(ecr_ovr_next))
  
  ecr_ovr_fit <- earth_spec %>% 
    fit(ecr_ovr_next ~ ., data = ecr_ovr_df)
  
  print(
    pull_workflow_fit(ecr_ovr_fit) %>%
      vip(geom = "point", num_features = 15) +
      ggtitle(glue("Variable Importance for MARS ",pos," ECR Model")) +
      geom_point(size = 2) +
      theme_minimal()
  )
  
  #ecr_pos
  ecr_pos_df <- df %>% 
    select(-c(ecr_ovr_next, parlay_td_next)) %>% 
    filter(!is.na(ecr_pos_next))
  
  ecr_pos_fit <- earth_spec %>% 
    fit(ecr_pos_next ~ ., data = ecr_pos_df)
  
  df %>%
    bind_cols(predict(ecr_ovr_fit, .)) %>% 
    rename(ecr_ovr_next_pred=.pred) %>% 
    bind_cols(predict(ecr_pos_fit, .)) %>% 
    rename(ecr_pos_next_pred=.pred)
}


# Tidymodels --------------------------------------------------------------
xgb_rec <- recipe(parlay_td_next ~  ., data=df_train) %>% 
  step_knnimpute(all_predictors(),
                 neighbors = 25)

prep_rec <- prep(xgb_rec)

xgb_spec <- multinom_reg(
  penalty = tune(),
  mixture = tune()
) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

xgb_workflow <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(xgb_rec)


xgb_grid <- expand_grid(
  mixture = seq(0.2,0.7,0.05),
  #penalty = seq(0.0,0.01,0.002)
  penalty = 0
)

xgb_res <- tune_grid(
  xgb_workflow,
  resamples = folds,
  grid = xgb_grid,
  metrics = metric_set(recall, precision),
  control = control_grid(save_pred = TRUE)
)

earth_workflow <- workflow() %>%
  add_model(earth_spec) %>%
  add_formula(as.factor(parlay_td_next) ~  .)

# Model data ----------------------------------------------------------------
library(furrr)

set.seed(1234)
memory.limit(size=50000)

ep_model_data <- ep_lagged_lines %>% 
  filter(Season >= 2006, Season < 2020, !is.na(parlay_td_next), Pos == "RB") %>% 
  select(-c(Week, Team, gsis_game_id, Name, gsis_id, Pos)) %>% 
  mutate(parlay_td_next = as.factor(parlay_td_next))

df_split <- initial_split(ep_model_data, prop = 4/5, strata = Season)

df_train <- training(df_split)
folds <- vfold_cv(df_train, 4, strata = Season)
df_test <- testing(df_split)

ep_model_data <- ep_lagged_lines %>%
  select(-c(Week, Team, gsis_game_id, Name, gsis_id)) %>%
  filter(Season >= 2006, Season < 2020, !is.na(parlay_td_next)) %>%
  group_by(Pos) %>% 
  nest() %>% 
  mutate(df_split = map(data, ~get_split(.x)),
         df_train = map(df_split, ~get_split_data(.x, "train")),
         df_test = map(df_split, ~get_split_data(.x, "test")),
         df_train_ecr = map2(df_train, Pos, ~get_ecr_predictions(.x, .y))
         
         model_fit = map(df_train, ~parsnip::fit(.env$earth_workflow, data=.x), .progress = TRUE),
         df_test_predict = map2(df_test, model_fit, add_pred),
         df_test_metrics = map(df_test_predict, get_metrics))


# Metrics -----------------------------------------------------------------

best1 <- show_best(xgb_res, "recall")
best2 <- show_best(xgb_res, "precision")

xgb_res %>%
  collect_metrics

xgb_res %>%
  collect_metrics %>% 
  #filter(mixture == 0.5) %>% 
  select(mixture, .metric, mean) %>% 
  ggplot(aes(mixture, mean, color = .metric)) +
  geom_point(alpha = 0.8, size = 2) +
  theme_minimal() +
  facet_wrap(~.metric, scales = "free_y")


xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "precision") %>%
  select(mean, penalty:mixture) %>%
  pivot_longer(penalty:mixture,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "precision")

best_auc <- select_best(xgb_res, "precision")

final_xgb <- finalize_workflow(
  xgb_workflow,
  best_auc
)

final_xgb %>%
  fit(data = df_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res <- last_fit(final_xgb, df_split)

collect_metrics(final_res)


df_train_fit <- fit(final_xgb, data = df_train)

df_train %>%
  bind_cols(predict(df_train_fit, df_train)) %>% 
  yardstick::precision(as.factor(parlay_td_next), .pred_class)

df_test %>%
  bind_cols(predict(df_train_fit, df_test)) %>% 
  yardstick::precision(as.factor(parlay_td_next), .pred_class)

temp2 <- df_test %>%
  bind_cols(predict(df_train_fit, df_test, type = "prob"))  %>% 
  mutate(across(where(is.numeric), round, 2)) %>%
  arrange(-.pred_1) %>% 
  select(.pred_1, .pred_0, parlay_td_next,
         implied_total_next, total_fp_x_roll2, rush_fp_roll16, rush_fp_roll8,
         rush_td_team_x_roll16, pass_att_team_roll16, rush_yd_team_x, total_yd_x, rush_yd_x_roll1)

gain_curve(temp2, as.factor(parlay_td_next), .pred_1) %>% autoplot()
lift_curve(temp2, as.factor(parlay_td_next), .pred_0) %>% autoplot()

# df_test %>%
#   bind_cols(predict(df_train_fit, df_test, type = "prob")) %>%
#   mutate(tdpred = ifelse(.pred_1>=0.5,1,0)) %>% 
#   conf_mat(as.factor(parlay_td_next), as.factor(tdpred)) %>% autoplot()

pr_curve(temp2, as.factor(parlay_td_next), .pred_0) %>% 
  ggplot(aes(x=.threshold)) +
  geom_point(aes(y=precision), color = "red") +
  geom_point(aes(y=recall), color = "black") +
  theme_minimal()

temp %>% 
  ggplot(aes(x=.threshold)) +
  geom_point(aes(y=specificity)) +
  geom_point(aes(y=sensitivity, color = "red"))

temp <- final_res %>%
  collect_predictions() %>%
  roc_curve(`as.factor(parlay_td_next)`, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )

temp2 %>% 
  mutate(tdpred = ifelse(.pred_1>=0.5,1,0)) %>% 
  group_by(tdpred) %>% 
  summarise(mean(.pred_1),
            mean(parlay_td_next),
            sum(parlay_td_next),
            n())

temp2 %>% 
  ggplot(aes(x=.pred_1)) +
  geom_histogram()

temp2 %>% 
  ggplot(aes(x=implied_total_next, y=pass_att_team_roll16, z = parlay_td_next_pred)) +
  stat_summary_hex() + 
  scale_color_gradient2(palette = "Greens") +
  theme_minimal() +
  geom_smooth() +
  facet_wrap("parlay_td_next")

test_rs %>%
  metrics(parlay_td_next, parlay_td_next_pred)


# Predict 2020 data -------------------------------------------------------

parlay_predictions_2020 <- ep_lagged_lines %>%
  filter(Pos == "RB", Season == 2020, Week < 10) %>% 
  bind_cols(predict(df_train_fit, new_data =  ., type = "prob")) %>%
  rename(parlay_td_next_pred = .pred_1) %>% 
  group_by(gsis_id) %>% 
  filter(Week == max(Week)) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), round, 2)) %>%
  select(Week, Team, gsis_game_id, Name, parlay_td_next_pred, parlay_td_next,
         implied_total_next, total_fp_x_roll2, rush_fp_roll16, rush_fp_roll8,
         rush_td_team_x_roll16, pass_att_team_roll16, rush_yd_team_x, total_yd_x, rush_yd_x_roll1)

parlay_predictions_2020 <- ep_lagged_lines %>%
  inner_join(games_combined, by = c("gsis_game_id" = "game_id")) %>% 
  filter(Pos == "RB") %>% 
  select(-c(Week, Team, gsis_game_id, Name, gsis_id, Pos))



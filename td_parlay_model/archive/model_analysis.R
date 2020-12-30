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
  mutate(across(where(is.numeric), round, 2)) %>%
  select(Season, Week, week_season, Team, gsis_game_id, Name, Pos, gsis_id, player_age, game_number,
         posteam_type_next, spread_line_next, total_line_next, parlay_td_next, implied_total_next, where(is.numeric), #contains("roll"),
         -contains("Season_"), -contains("Week_"), -contains("week_season_num_"), -contains("player_age_"),
         -contains("game_number_"), -contains("next_roll"))

write_arrow(ep_lagged_lines, "model_roll.pdata")
ep_lagged_lines <- read_arrow("model_roll.pdata")

# Modeling ----------------------------------------------------------------
set.seed(1234)
memory.limit(size=50000)

ep_model_data <- ep_lagged_lines %>% 
  filter(Season >= 2006, Season < 2020, !is.na(parlay_td_next), Pos == "WR") %>% 
  select(-c(Week, Team, gsis_game_id, Name, gsis_id, Pos))

df_split <- initial_split(ep_model_data, prop = 4/5, strata = Season)

df_train <- training(df_split)
folds <- vfold_cv(df_train, 4)
df_test <- testing(df_split)

# new_DF <- ep_model_data[rowSums(is.na(ep_model_data)) > 0,]

earth_spec <- multinom_reg(
  penalty = tune(),
  mixture = tune()) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

earth_grid <- grid_regular(
  num_terms(range = c(6,20)),
  levels = 5
)

earth_tune <- earth_workflow %>%
  tune_grid(resamples = folds,
            grid = earth_grid)

earth_tune %>%
  collect_metrics %>%
  filter(.metric == "roc_auc") %>%
  ggplot(aes(num_terms, mean))+
  geom_point()

# rec_wf <- rec_wf %>%
#   finalize_workflow(tibble(num_terms = 14))

# earth_spec <- mars(
#   num_terms = 15,
#   prod_degree = 2,
#   prune_method = "exhaustive") %>%
#   set_mode("classification") %>%
#   set_engine("earth")

earth_workflow <- workflow() %>%
  add_model(earth_spec) %>%
  add_formula(as.factor(parlay_td_next) ~  .)



earth_fit <- fit(earth_workflow, data = df_train)

pull_workflow_fit(earth_fit) %>%
  vip(geom = "point", num_features = 10) +
  ggtitle("Variable Importance for MARS RB TD Model") +
  geom_point(size = 2) +
  theme_minimal()

earth_fit_summ <- earth_fit$fit$fit$fit
# earth_fit_summ <- earth_fit$fit$fit$fit$model_df$model[[1]]$fit
summary(earth_fit_summ)
plot(earth_fit_summ)

earth_fit_pred <- earth_fit$pre$mold$predictors

pdp::partial(earth_fit_summ, pred.var = c("total_yd_x_roll2"), train = earth_fit_pred) %>% autoplot()
pdp::partial(earth_fit_summ, pred.var = c("total_yd_x_roll2", "implied_total_next"), train = earth_fit_pred) %>%
  autoplot() +
  theme_minimal() +
  labs(title = "")

test_rs <- df_train %>%
  bind_cols(predict(earth_fit, df_train, type = "prob")) %>%
  rename(parlay_td_next_pred = .pred_1)

test_rs %>% 
  ggplot(aes(x=parlay_td_next_pred)) +
  geom_histogram()

test_rs %>% 
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
  filter(Pos == "WR", Season == 2020, Week < 10) %>% 
  bind_cols(predict(earth_fit, new_data =  ., type = "prob")) %>%
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


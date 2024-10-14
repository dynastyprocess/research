library(hoopR)
library(tidyverse)
library(slider)

nba_name_mapper <- function(name){
  switch(name,
         "Jose Juan Barea" = "JJ Barea",
         "Nene Hilario" = "Nene",
         "Wesley Iwundu" = "Wes Iwundu",
         "Cameron Thomas" = "Cam Thomas",
         "Jeff Pendergraph" = "Jeff Ayres",
         "Nicolas Claxton" = "Nic Claxton",
         "NahShon Hyland" = "Bones Hyland",
         "Mohamed Bamba" = "Mo Bamba",
         "Juan Hernangomez" = "Juancho Hernangomez",
         "Patrick Mills" = "Patty Mills",
         "Sviatoslav Mykhailiuk" = "Svi Mykhailiuk",
         "Louis Amundson" = "Lou Amundson",
         "Nando de Colo" = "Nando De Colo",
         "Robbie Hummel" = "Robbie John Hummel",
         name)
}

get_rate <- function(x, y){
  rate <- sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE)
  
  ifelse(is.nan(rate) | is.infinite(rate), 0, rate)
}

numberfire_projections <- read.csv(file = "numbefire_2013_2023.csv")

week_ranges <- 
  tibble(date = as_date("2013-10-01"):as_date("2023-11-12")) |> 
  mutate(date = as_date(date),
         day_of_week = wday(date, label = TRUE)) |> 
  filter(day_of_week %in% c("Mon","Sun")) |> 
  mutate(week_end = lead(date)) |> 
  filter(day_of_week == "Mon")

numberfire_projections_range <- 
  numberfire_projections |> 
  mutate(date = as_date(date)) |> 
  left_join(week_ranges |> 
              select(date, week_end),
            by = c("date")) |> 
  transmute(week_start = date,
            week_end,
            name,
            pos,
            team,
            player_name = nflreadr::clean_player_names(name),
            player_name = map_chr(player_name, nba_name_mapper),
            proj_points = pts / g,
            proj_min = min / g,
            proj_fgm = fgm / g,
            proj_fga = fga / g,
            proj_fg_pct = fg_percent,
            proj_ftm = ftm / g,
            proj_fta = fta / g,
            proj_ft_pct = ft_percent,
            proj_3pm = x3pm / g,
            proj_3pa = x3pa / g,
            proj_3p_pct = x3p_percent,
            proj_reb = reb / g,
            proj_ast = ast / g,
            proj_stl = stl / g,
            proj_blk = blk / g,
            proj_tov = tov / g)

nba_player_box <- 
  hoopR::load_nba_player_box(2013:2024) |> 
  transmute(season,
         game_id,
         game_date,
         athlete_display_name,
         player_name = nflreadr::clean_player_names(athlete_display_name),
         team_display_name,
         team_abbreviation,
         opponent_team_display_name,
         opponent_team_abbreviation,
         minutes,
         field_goals_made,
         field_goals_attempted,             
         three_point_field_goals_made,      
         three_point_field_goals_attempted, 
         free_throws_made,                  
         free_throws_attempted,             
         offensive_rebounds,                
         defensive_rebounds,                
         rebounds,                         
         assists,                           
         steals,                            
         blocks,                        
         turnovers,                         
         fouls,
         points,
         starter,
         ejected,
         did_not_play,
         active,
         athlete_position_abbreviation,
         home_away,
         team_winner,
         team_score,
         opponent_team_score)

every_game <- 
  nba_player_box |> 
  distinct(game_date,
           team_abbreviation,
           season)

every_player_team <- 
  nba_player_box |> 
  distinct(player_name,
           team_abbreviation,
           season) |> 
  left_join(every_game, by = c("season", "team_abbreviation"))

proj_box <- 
  nba_player_box |> 
  full_join(every_player_team, 
            by = c("player_name", "season", "team_abbreviation", "game_date")) |> 
  left_join(numberfire_projections_range,
            by = join_by(player_name,
                         team_abbreviation == team,
                         between(game_date, week_start, week_end)))

missing_assists <- 
  proj_box |> 
  filter(is.na(minutes) | minutes == 0) |> 
  summarise(missing_min = sum(proj_min, na.rm = TRUE),
            missing_ast = sum(proj_ast, na.rm = TRUE),
            .by = c(game_date, team_abbreviation))

proj_box |> 
  filter(team_abbreviation == "MIA",
         season == 2024) |> 
  View()

# proj_box |>
#   summarise(missing_mins = sum(if_else(is.na(proj_tov),
#                                        minutes,
#                                        0),
#                                na.rm = TRUE),
#             has_mins = sum(if_else(is.na(proj_tov),
#                                    0,
#                                    minutes),
#                            na.rm = TRUE),
#             first_game = min(game_date),
#             last_game = max(game_date),
#             .by = c(player_name, team_abbreviation)) |>
#   View()

# corrr::correlate(proj_box) |> 
#   corrr::focus(minutes) |> 
#   View()



prep_data <- 
  proj_box |>
  left_join(missing_assists, by = c("game_date", "team_abbreviation")) |> 
  filter(minutes > 0) |>
  arrange(season, game_date, team_abbreviation, -proj_ast) %>%
  group_by(team_abbreviation, game_date) %>% 
  mutate(
    team_assists = sum(assists, na.rm = TRUE),
    team_proj_ast = sum(proj_ast, na.rm = TRUE),
    proj_assist_share = proj_ast / team_proj_ast,
    teammate_gap_to_best_ast = proj_ast[1] - proj_ast,
    teammate_gap_to_2nd_best_rank = proj_ast[2] - proj_ast,
    teammate_gap_to_3rd_best_rank = proj_ast[3] - proj_ast,
    teammate_gap_to_4rd_best_rank = proj_ast[4] - proj_ast,
    teammate_gap_to_5th_best_rank = proj_ast[5] - proj_ast,
    
    # teammate_gap_to_2nd_best_rank = if_else(is.na(teammate_gap_to_2nd_best_rank), 0 - assists_roll8, teammate_gap_to_2nd_best_rank),
    # teammate_gap_to_3rd_best_rank = if_else(is.na(teammate_gap_to_3rd_best_rank), 410 - assists_roll8, teammate_gap_to_3rd_best_rank),
    teammate_gap_to_next_best_rank = proj_ast - lag(proj_ast, default = NA)
  ) %>% 
  ungroup() %>% 
  mutate(across(.cols = c(minutes, assists, fouls, turnovers),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
                .names = "{.col}_roll8"),
         assist_share_roll8 = slide2_dbl(assists,
                                         team_assists,
                                         ~get_rate(.x,.y),
                                         .before = 8,
                                         .after = -1),
         proj_assist_share_roll8 = slide2_dbl(proj_ast,
                                              team_proj_ast,
                                              ~get_rate(.x,.y),
                                              .before = 8,
                                              .after = -1),
         .by = c(season, player_name)) |> 

  
  filter(!is.na(proj_ast),
         !is.na(assists_roll8)) |> 
  transmute(season,
         player_name,
         team_abbreviation,
         home_away,
         athlete_position_abbreviation,
         assists,
         assists_factor = case_when(assists < 12 ~ as.character(assists),
                                    assists >= 12 ~ "12+"),
         starter = as.factor(starter),
         proj_ast,
         proj_min,
         proj_tov,
         proj_assist_share,
         missing_min,
         missing_ast,
         minutes_roll8,
         assists_roll8,
         assist_share_roll8,
         proj_assist_share_roll8,
         proj_assist_share_diff = proj_assist_share - proj_assist_share_roll8,
         fouls_roll8,
         turnovers_roll8,
         minutes_diff = proj_min - minutes_roll8,
         assists_diff = proj_ast - assists_roll8,
         turnovers_diff = proj_tov - turnovers_roll8
         
         )
  

# Split Train/Test Data ---------------------------------------------------
library(tidymodels)
library(finetune)

set.seed(815)


rec_train <- prep_data %>%
  filter(season <= 2023, season >= 2018)

training_resamples <- validation_split(rec_train)

rec_test <- prep_data %>%
  filter(season >= 2024)


# XGBoost Reception Model -------------------------------------------------
assists_recipe <- 
  recipe(assists_factor ~ ., data = rec_train) %>%
  update_role(c(season, player_name, team_abbreviation), new_role = "id") %>% 
  step_rm(c(assists)) %>% 
  step_dummy(c(home_away), one_hot = FALSE) %>%
  step_dummy(c(starter), one_hot = FALSE) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

prepped <- prep(assists_recipe)
juiced <- juice(prepped)
baked <- bake(prepped, rec_train)
skimr::skim(baked)
levels(baked$assists_factor)

assists_boost <- 
  boost_tree(mode = "classification",
             mtry = tune(),  
             trees = tune(),
             min_n = tune(),
             stop_iter = tune(),
             tree_depth = tune(),
             learn_rate = tune(),
             loss_reduction = tune(),
             sample_size = tune()) %>% 
  set_engine(engine = "xgboost",
             eval_metric = 'mlogloss',
             validation = 0.2
  )

assists_wf <- workflow(assists_recipe, assists_boost)

new_params <-
  hardhat::extract_parameter_set_dials(assists_wf) %>%
  update(mtry = mtry(range = c(2,15)),
         min_n = min_n(range = c(30, 5000)),
         tree_depth = tree_depth(range = c(3,15)),
         trees = trees(range = c(500,2000)),
         loss_reduction = loss_reduction(range = c(-10, 2), trans = scales::log10_trans()),
         learn_rate = learn_rate(range = c(-3, -0.5), trans = scales::log10_trans()))

all_cores <- parallelly::availableCores() - 1
future::plan("multisession", workers = all_cores)

bayes_tune_xgb_assists <- 
  assists_wf %>% 
  tune_bayes(
    resamples = training_resamples,
    metrics = metric_set(mn_log_loss),
    initial = 5,
    control = control_bayes(
      verbose = TRUE,
      no_improve = 20,
      uncertain = 10,
      seed = 815,
      save_pred = TRUE,
      parallel_over = 'everything',
      save_workflow = TRUE
    ),
    param_info = new_params,
    iter = 50
  )

bayes_tune_xgb_assists<- 
  assists_wf %>% 
  tune_bayes(
    resamples = training_resamples,
    metrics = metric_set(mn_log_loss),
    initial = 5,
    control = stacks::control_stack_bayes(),
    param_info = new_params,
    iter = 50
  )

saveRDS(bayes_tune_xgb_assists, "./models_xgboost/bayes_tune_assists.RDS")

autoplot(bayes_tune_xgb_assists, type = "performance") + theme_minimal()
autoplot(bayes_tune_xgb_assists, type = "parameters") + theme_minimal()
autoplot(bayes_tune_xgb_assists, 
         rank_metric = "mn_log_loss",
         metric = "mn_log_loss",
         select_best = FALSE) +
  theme_minimal() +
  ylim(1.7,2.1)

bayes_tune_xgb_assists %>% collect_metrics() %>% View()

best_tree <- bayes_tune_xgb_assists %>%
  select_best("mn_log_loss")

final_wf <- 
  assists_wf %>% 
  finalize_workflow(best_tree)

final_fit <- fit(final_wf, rec_train)

# saveRDS(final_fit, "/rnddrive/JoeS/receptions_fit_v3.RDS")
saveRDS(final_fit, "./models_xgboost/assists_fit_v1.RDS")

assists_explainer <-
  DALEXtra::explain_tidymodels(
    final_fit,
    data = select(rec_train, -assists_factor),
    y =  rec_train %>% pull(assists_factor)
  )

# saveRDS(rec_completion_explainer, "./models_xgboost/rec_fit_explainer_v2.RDS")

feat_imp <- DALEX::feature_importance(assists_explainer, N = 500)

feat_vars <- feat_imp %>% 
  group_by(variable) %>% 
  summarise(max_dropout = max(dropout_loss)) %>% 
  ungroup() %>% 
  slice_max(order_by = max_dropout, n = 25) %>% 
  bind_rows(tibble(variable = "_full_model_"))

plot(feat_imp)

plot(DALEX::model_profile(assists_explainer,
                          variables = c("proj_tov"),
                          N = 500))


# Scratch paper -----------------------------------------------------------



nba_team_box <- 
  hoopR::load_nba_team_box(2023)






nba_player_box_24 <- hoopR::load_nba_player_box(2024)

rolling_averages <- 
  nba_player_box_24 |> 
  arrange(game_date) |> 
  mutate(across(.cols = c(minutes, steals, turnovers),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
                .names = "{.col}_roll4"),
         .by = c(season, athlete_display_name))

rolling_averages |> 
  filter(team_name == "Celtics") |> 
  View()


nba_team_sched <- load_nba_schedule(seasons = 2023)

nba_play_by_play <- load_nba_pbp(2023)
nba_pbp <- nba_play_by_play %>% filter(game_id == 401468016)
nba_pbp2 <- espn_nba_pbp(game_id = 401468016)

nba_betting_data <- espn_nba_betting(401442535)

nba_player_track <- nba_boxscoreplayertrackv2(401468615)


test <- nba_data_pbp(game_id = "0021900001")


fanduel <- nbastatR::fanduel_summary(
  game_ids = c(21700002, 21700003),
  nest_data = FALSE,
  return_message = TRUE
)


swish <- 
  "https://swishanalytics.com/optimus/nba/ajax/daily-fantasy-projections-ajax.php"

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0'
)

date <- "2023-11-11"

x <- httr::POST(url = "https://swishanalytics.com/optimus/nba/ajax/daily-fantasy-projections-ajax.php",
                body = list(date = date),
                httr::add_headers(.headers=headers))

y <- content(x, as = "text") %>% jsonlite::parse_json()


new_dfs <- 
  tibble(player = y$players) %>% 
  unnest_wider(player) |> 
  unnest_wider(salaries)



require(httr)


headers = c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/119.0"
)

params = list(
  `team` = "IND"
)

res <- httr::GET(url = "https://www.rotowire.com/basketball/ajax/get-projected-minutes.php",
                 httr::add_headers(.headers=headers),
                 query = params)

minutes_proj <- content(res, as = "text") %>% jsonlite::parse_json()

minutes_proj |> tibble() |> unnest_wider(minutes_proj)

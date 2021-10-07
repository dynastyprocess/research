library(tidyverse)

pred_df <- read_csv("https://raw.githubusercontent.com/nflverse/nfldata/master/data/predictions.csv")

games <- 
  nflreadr::load_schedules(seasons = 2021) %>% 
  transmute(game_id, away_team, home_team, winning_team = if_else(result > 0, home_team, away_team))

joe <- pred_df %>% 
  filter(screen_name %in% c("JoeSydlowskiFF","Market")) %>% 
  left_join(games, by = "game_id") %>% 
  mutate(winning_team_pred = if_else(prediction > 50, home_team, away_team),
         winning_team_prob = if_else(home_team == winning_team_pred, prediction, 100-prediction),
         correct_pred = if_else(winning_team == winning_team_pred, 1, 0),
         brier = 25 - (100 * (winning_team_prob/100 - correct_pred)**2),
         brier = if_else(prediction == 0, 0, brier)) %>% 
  pivot_wider(id_cols = c(game_id, home_team, away_team, winning_team),
              names_from = screen_name,
              values_from = c(prediction, winning_team_pred, winning_team_prob, correct_pred, brier)) %>% 
  mutate(favored_side = case_when(
    brier_Market == 0 ~ "Even",
    winning_team_pred_JoeSydlowskiFF == winning_team_pred_Market &
      winning_team_prob_JoeSydlowskiFF > winning_team_prob_Market ~ "Fav",
    winning_team_pred_JoeSydlowskiFF != winning_team_pred_Market |
      winning_team_prob_JoeSydlowskiFF < winning_team_prob_Market ~ "Dog",   
    TRUE ~ "Err"))


joe %>% 
  group_by(favored_side, correct_pred_JoeSydlowskiFF) %>% 
  summarise(sum(brier_JoeSydlowskiFF),
            n()) %>% 
  ungroup

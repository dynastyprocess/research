library(ffscrapr)
library(tidyverse)

get_scoring_proj <- function(season_num, week_num){
  
  conn <- mfl_connect(season_num, 54040, user_agent = "dynastyprocess",  
                      #user_name = "syd12nyjets", password = "Jojo18embiid",
                      rate_limit_number = 5, rate_limit_seconds = 60)
  
  players <- mfl_players(conn) %>% 
    select(player_id, player_name, pos)
  
  proj <- mfl_getendpoint(conn, "projectedScores", W=week_num, YEAR=season_num) %>%
    pluck("content","projectedScores","playerScore") %>%
    tibble() %>% 
    unnest_wider(1) %>%
    rename(player_id = id, projected_score = score)

    ffscrapr::ff_starters(conn, season = season_num, week = week_num) %>%
      select(-player_name, -pos) %>% 
      left_join(proj, by = "player_id") %>% 
      left_join(players, by = "player_id")
  
}

grid_df <-
  expand_grid(
    season = c(2018:2020),
    week = c(1:13)) %>%
  mutate(week_scores = map2(season, week, ~get_scoring_proj(.x,.y)))

df_expand <- 
  grid_df %>%
  select(-week, -season) %>% 
  unnest(week_scores) %>%
  mutate(projected_score = as.numeric(projected_score),
         projected_score = replace_na(projected_score, 0),
         player_score = replace_na(player_score, 0),
         
         flex = if_else(pos == "QB", "N", "Y")) %>% 
  group_by(franchise_name, season, week, pos) %>%
  mutate(pos_rank = row_number(-projected_score)) %>%
  ungroup() %>% 
  group_by(franchise_name, season, week, flex) %>%
  mutate(flex_rank = row_number(-if_else(pos_rank == 1, -999, projected_score))) %>% 
  ungroup() %>% 
  mutate(woulda_started = if_else(pos_rank == 1 | (pos != "QB" & flex_rank <= 5), "Y", "N"))


# Find Replacement Level --------------------------------------------------
baselines <- df_expand %>% 
  filter(starter_status == "starter") %>% 
  group_by(pos) %>% 
  summarise(week_starters = n() / 39) %>% 
  ungroup()


# By Franchise ------------------------------------------------------------
team_week <- df_expand %>% 
  group_by(franchise_name, season, week) %>% 
  summarise(actual_score = sum(if_else(starter_status == "starter", player_score, 0), na.rm = TRUE),
            proj_score = sum(if_else(woulda_started == "Y", player_score, 0), na.rm = TRUE),
            best_ball_gap = actual_score - proj_score) %>% 
  ungroup() %>% 
  group_by(franchise_name) %>% 
  mutate(week_num = row_number()) %>% 
  ungroup()

team_week %>%
  filter(franchise_name == "Team Pikachu") %>% 
  ggplot(aes(x=week_num, y=best_ball_gap)) +
  geom_point() +
  geom_segment(aes(x=week_num, xend = week_num, y = 0, yend = best_ball_gap)) +
  facet_wrap(~pos)

team_week %>% 
  group_by(franchise_name) %>% 
  summarise(avg_best_ball_gap = mean(best_ball_gap)) %>% 
  arrange(-avg_best_ball_gap)


# By Position -------------------------------------------------------------
pos_week <- df_expand %>% 
  group_by(franchise_name, season, week, pos) %>% 
  summarise(actual_score = sum(if_else(starter_status == "starter", player_score, 0), na.rm = TRUE),
            proj_score = sum(if_else(woulda_started == "Y", player_score, 0), na.rm = TRUE),
            best_ball_gap = actual_score - proj_score) %>% 
  ungroup() %>% 
  group_by(franchise_name, pos) %>% 
  mutate(week_num = row_number()) %>% 
  ungroup()

pos_week %>%
  filter(franchise_name == "Team Link") %>% 
  ggplot(aes(x=week_num, y=actual_score)) +
  geom_point() +
  geom_segment(aes(x=week_num, xend = week_num, y = 0, yend = actual_score)) +
  theme_minimal() +
  labs(title = "Team Link Bestball Gaps by Position over time") +
  facet_wrap(~pos)

pos_week %>% 
  group_by(franchise_name, pos) %>% 
  summarise(avg_best_ball_gap = mean(best_ball_gap)) %>% 
  arrange(-avg_best_ball_gap)

pos_week %>% 
  group_by(franchise_name, pos) %>% 
  summarise(avg_actual_score = mean(actual_score)) %>% 
  arrange(-avg_actual_score) %>% 
  view()

# By Player -------------------------------------------------------------
player_week <- df_expand %>% 
  group_by(franchise_name, season, week, pos, player_name, player_id) %>% 
  summarise(actual_score = sum(if_else(starter_status == "starter", player_score, 0), na.rm = TRUE),
            proj_score = sum(if_else(woulda_started == "Y", player_score, 0), na.rm = TRUE),
            best_ball_gap = actual_score - proj_score) %>% 
  ungroup() %>% 
  group_by(franchise_name, pos) %>% 
  mutate(week_num = row_number()) %>% 
  ungroup()

df_expand %>% filter(woulda_started == "N", starter_status == "starter", season == 2020) %>% group_by(franchise_name, player_name) %>% summarise(n = n(), bestball_gap = sum(player_score - projected_score)) %>% view()
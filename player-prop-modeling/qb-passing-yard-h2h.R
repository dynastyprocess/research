
library(tidyverse)
library(nflreadr)

pbp <- load_pbp(2016:2022)


closing_lines <- load_schedules(seasons = 2016:2022) %>% 
  filter(total_line >= 50, total_line <=58, spread_line >= -3.5, spread_line <= 3.5)

pbp_filtered <- pbp %>% 
  filter(play_type == "pass", complete_pass == 1) %>% 
  group_by(season, week, game_id, team = clean_team_abbrs(posteam), passer_player_name) %>% 
  summarise(total_passing_yards = sum(yards_gained)) %>% 
  ungroup() %>% 
  group_by(season, week, game_id, team) %>% 
  slice_max(order_by = total_passing_yards, n = 1) %>% 
  ungroup()


joined <- 
  closing_lines %>% 
  select(season, week, game_id, home_team, away_team, spread_line) %>% 
  pivot_longer(cols = c(home_team, away_team)) %>% 
  mutate(favorite = case_when(name == "away_team" & spread_line < 0 ~ "yes",
                              name == "away_team" & spread_line > 0 ~ "no",
                              name == "home_team" & spread_line > 0 ~ "yes",
                              name == "home_team" & spread_line < 0 ~ "no",
                              TRUE ~ "tie"
                              )) %>% 
  mutate(team = clean_team_abbrs(value)) %>% 
  left_join(pbp_filtered, by = c("season", "week", "team", "game_id"))

joined_pivot <- joined %>% 
  pivot_wider(names_from = c(favorite),
              id_cols = c(season, week, game_id, spread_line),
              values_from = c(total_passing_yards)
  )


joined_pivot %>% 
  group_by(yes > no) %>% 
  tally()


game_stats <- load_player_stats(2016:2022)

game_stats %>% 
  filter(season >= 2016) %>% 
  filter(player_display_name %in% c("Travis Kelce", "Stefon Diggs")) %>% 
  group_by(player_name) %>% 
  summarise(sum(rushing_tds + receiving_tds > 0)/ n())

game_stats %>% 
  filter(season >= 2016) %>% 
  filter(player_display_name %in% c("Justin Herbert")) %>% 
  group_by(player_name) %>% 
  summarise(sum(passing_tds > 0), n())

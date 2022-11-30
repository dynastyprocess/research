
# Import Libraries --------------------------------------------------------
library(tidyverse)
library(nflreadr)
library(slider)
library(directlabels)

fantasy_data <- nflreadr::load_player_stats(seasons = c(2011:2022)) %>%
  filter(season_type == "REG")

baselines <- fantasy_data %>%
  # filter((season<=2020 & week <= 17) | (season >= 2021 & week <= 18)) %>%
  group_by(season, week, position = if_else(position == "FB", "RB", position)) %>%
  mutate(pos_rank = row_number(desc(fantasy_points_ppr))) %>%
  ungroup() %>%
  filter(pos_rank %in% c(6,12,24,36,48)) %>%
  arrange(position, season, week) %>%
  group_by(position, pos_rank) %>%
  mutate(avg_baseline_prev16 = slide_dbl(fantasy_points_ppr, ~mean(.x, na.rm =TRUE), .before = 15, .after = 0),
         weekly_baseline = fantasy_points_ppr) %>%
  ungroup() %>%
  select(season, week, position, pos_rank, player_name, fantasy_points_ppr, avg_baseline_prev16, weekly_baseline)

baseline_joinprep <-
  expand_grid(season = baselines %>% select(season) %>% distinct() %>% pull(),
              week = baselines %>% select(week) %>% distinct() %>% pull(),
              position = baselines %>% select(position) %>% distinct() %>% pull(),
              pos_rank = baselines %>% select(pos_rank) %>% distinct() %>% pull()) %>%
  left_join(baselines, by = c("season", "week", "position", "pos_rank")) %>%
  mutate(avg_baseline_prev16 = replace_na(avg_baseline_prev16,0)) %>%
  select(season, week, position, pos_rank, avg_baseline_prev16, weekly_baseline)

game_df <- fantasy_data %>%
  left_join(baseline_joinprep, by = c("season","week","position")) %>%
  group_by(player_id, position, pos_rank) %>%
  mutate(fpob_roll16  = slide_dbl(fantasy_points_ppr - avg_baseline_prev16, ~mean(.x, na.rm =TRUE), .before = 15, .after = 0),
         total_fp_roll16  = slide_dbl(fantasy_points_ppr, ~mean(.x, na.rm =TRUE), .before = 15, .after = 0),
         weekly_points_above_baseline = fantasy_points_ppr - weekly_baseline,
         game_number = row_number(),
         death_week = max(replace_na(if_else(fpob_roll16 > 0, game_number, 0L),0)),
         games_remaining = death_week - game_number,
         games_remaining = if_else(games_remaining<0,0L,games_remaining),
         weeks_above = sum(fpob_roll16>0),
         weeks_above_todate = cumsum(fpob_roll16>0),
         weeks_above_remaining = weeks_above - weeks_above_todate,
         active_2022 = sum(season == 2022)) %>%
  ungroup() %>%
  filter(season >= 2012) %>%
  select(season,
         week,
         player_name = player_display_name,
         position,
         fantasy_points_ppr,
         total_fp_roll16,
         weekly_baseline,
         weekly_points_above_baseline,
         fpob_roll16,
         pos_rank,
         avg_baseline_prev16,
         game_number,
         death_week,
         weeks_above,
         weeks_above_todate,
         weeks_above_remaining,
         active_2022)

#Player Plot
game_df %>%
  filter(player_name == "Cooper Kupp") %>%
  mutate(pos_rank = as.factor(pos_rank)) %>%
  ggplot(aes(x = game_number, group = pos_rank, color = pos_rank)) +
  geom_path(aes(y=avg_baseline_prev16), alpha = 0.7) +
  #ggrepel::geom_text_repel(aes(y=total_fp_roll16, label = games_remaining)) +
  #scale_color_brewer(palette = "RdYlGn", direction = -1) +
  scale_color_manual(values = c("#5aae61","#a6dba0","#f7f7f7","#cda5cf","#9970ab")) +
  geom_path(aes(y=total_fp_roll16), color = 'white', size = 1.5) +
  labs(title = "Top 6/12/24/36/48 baselines",
       subtitle = game_df %>% filter(player_name == "Cooper Kupp") %>% select(player_name) %>% unique() %>% pull(),
       x= "Games Played",
       y = "Average PPG Fantasy Points") +
  geom_dl(aes(y=avg_baseline_prev16 , label = pos_rank), color = "white", method = "last.points") +
  scale_x_continuous(breaks = seq(0,max(game_df$game_number),16))+
  hrbrthemes::theme_modern_rc(base_size = 14) +
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.position = "none")



game_df %>%
  group_by(player_name, position, pos_rank) %>%
  summarise(
    sum(fpob_roll16),
    sum(weekly_points_above_baseline),
    mean(fpob_roll16),
    mean(weekly_points_above_baseline),
    min(season),
    max(season),
    games = n()
  ) %>%
  ungroup() %>%
  view()


baseline %>%
  filter(season >= 2012) %>%
  group_by(season, week) %>%
  mutate(game_number = cur_group_id()) %>%
  ungroup() %>%
  mutate(pos_rank = as.factor(pos_rank)) %>%
  ggplot(aes(x = game_number, group = pos_rank, color = pos_rank)) +
  geom_path(aes(y=avg_baseline_prev16), alpha = 0.7) +
  labs(title = "Top 6/12/24/36/48 baselines",
       x= "Season",
       y = "Average PPG Fantasy Points") +
  theme_minimal() +
  facet_wrap(~position) +
  geom_dl(aes(y=avg_baseline_prev16 , label = pos_rank), color = "black", method = "last.points") +
  scale_x_continuous(breaks = seq(0,max(game_df$game_number),17),
                     labels = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))+
  theme(axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0, hjust=-0.5),
        legend.position = "none")


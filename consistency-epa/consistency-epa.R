library(nflreadr)
library(tidyverse)

nflfastr_rosters <-
  nflfastR::fast_scraper_roster(2019:2021) %>%
  select(season, gsis_id, position, full_name, birth_date, sportradar_id)

pbp <- 
  nflreadr::load_pbp(2019:2021)

pbp_ck <- 
  pbp %>% 
  # filter(qb_dropback == 1) %>% 
  # mutate(player_id = if_else(pass_attempt == 1, passer_player_id, rusher_player_id)) %>% 
  # left_join(nflfastr_rosters,
  #           by = c("player_id" = "gsis_id", "season"),
  #           na_matches = "never") %>%
  
  left_join(nflfastr_rosters,
            by = c("passer_player_id" = "gsis_id", "season"),
            na_matches = "never") %>%

  filter(position == "QB", game_half %in% c("Half1", "Half2")) %>% 
  group_by(posteam, position, full_name, game_id, game_half) %>% 
  summarise(epa_play = mean(epa, na.rm = TRUE),
            plays = n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(posteam, full_name, game_id),
              names_from = game_half,
              values_from = c(epa_play, plays)) %>% 
  filter(!is.na(plays_Half1), !is.na(plays_Half2), plays_Half1 >= 5, plays_Half2 >= 5) %>% 
  group_by(full_name) %>% 
  filter(n() >= 16) %>% 
  ungroup()

pbp_ck %>% 
  ggplot(aes(x = epa_play_Half1, y = epa_play_Half2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(~full_name) +
  scale_x_continuous(breaks = seq(-2, 2, by = 1)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 1)) +
  
  ggpubr::stat_cor(aes(label = ..rr.label..), color = "red", geom = "text")

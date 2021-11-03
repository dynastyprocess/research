library(nflreadr)
library(tidyverse)

snaps <- load_snap_counts() %>% 
  group_by(pfr_player_id) %>% 
  summarise(across(contains("snaps"), ~sum(.x, na.rm = TRUE))) %>% 
  ungroup()

rosters <- load_rosters()

stats <- load_player_stats() %>% 
  group_by(player_id) %>% 
  summarise(across(c(targets, receptions, receiving_yards, receiving_air_yards, fantasy_points_ppr),
                   ~sum(.x, na.rm = TRUE))) %>% 
  ungroup()

picks <- load_draft_picks() %>% 
  select(pfr_id, pick)

snaps_rosters <- rosters %>% 
  left_join(snaps, by = c("pfr_id" = "pfr_player_id")) %>% 
  left_join(picks, by = "pfr_id") %>% 
  left_join(stats, by = c("gsis_id" = "player_id")) %>% 
  select(full_name, pick, position, years_exp, targets, receptions, receiving_yards, receiving_air_yards, fantasy_points_ppr,
         offense_snaps)

library(nflfastR)
library(tidymodels)
library(stringr)
library(slider)

#Grab roster data
future::plan("multisession")
roster <- nflfastR::fast_scraper_roster(2015:2020)
future::plan("sequential")

#Calculate different scoring systems and pivot data
game_df <- 
  nflfastR::load_player_stats() %>%
  left_join(roster, by = c("player_id" = "gsis_id", "season" = "season")) %>% 
  filter(season >= 2015, week <= 16) %>%
  mutate(
    flex_eligible = case_when(position %in% c("RB","WR","TE") ~ "Y", TRUE ~ "N"),
    fantasy_points_te15 = fantasy_points_ppr + case_when(position == "TE" ~ 0.5, TRUE ~ 0) * receptions,
    fantasy_points_te2 = fantasy_points_ppr + case_when(position == "TE" ~ 1, TRUE ~ 0) * receptions
  ) %>% 
  group_by(season, week, position) %>%
  mutate(position_rank_ppr = row_number(desc(fantasy_points_ppr)),
         position_rank_te15 = row_number(desc(fantasy_points_ppr)),
         position_rank_te2 = row_number(desc(fantasy_points_ppr))) %>%
  ungroup() %>%
  group_by(season, week, flex_eligible) %>%
  mutate(flex_rank_ppr = row_number(desc(fantasy_points_ppr)),
         flex_rank_te15 = row_number(desc(fantasy_points_te15)),
         flex_rank_te2 = row_number(desc(fantasy_points_te2))) %>%
  ungroup() %>%
  select(player_id, player_name, position, season, week, contains("rank"), contains("fantasy_points")) %>% 
  pivot_longer(cols = c(contains("rank"), contains("fantasy_points_"))) %>% 
  separate(name, c("metric_type", "metric_type2", "scoring_type"), sep = "_") %>% 
  unite("metric_type", c(metric_type, metric_type2)) %>% 
  pivot_wider(id_cols = c(player_id, player_name, position, season, week, scoring_type),
              names_from = "metric_type",
              values_from = "value")

## Pull out the baselines
# baseline_prep <- 
#   game_df %>% 
#   filter(flex_rank == 84) %>%
#   arrange(scoring_type, season, week) %>% 
#   group_by(scoring_type) %>%
#   mutate(baseline_previous16 = slide_dbl(fantasy_points, ~mean(.x, na.rm =TRUE), .before = 15, .after = 0),
#          week_num = row_number()) %>%
#   ungroup() %>%
#   filter(season >= 2016) 

baseline_prep <- 
  game_df %>% 
  filter((position == "QB" & flex_rank == 12) | (position != "QB" & flex_rank == 84)) %>%
  arrange(flex_rank, scoring_type, season, week) %>% 
  group_by(flex_rank, scoring_type) %>%
  mutate(baseline_previous16 = slide_dbl(fantasy_points, ~mean(.x, na.rm =TRUE), .before = 15, .after = 0),
         week_num = row_number()) %>%
  ungroup() %>%
  filter(season >= 2016) # Leave 2015 in up to here for a run in period on the slider

#Visually check the baselines
baseline_prep %>%
  arrange(season, week) %>% 
  mutate(scoring_type = case_when(position == "QB" ~ "QB",
                                  TRUE ~ scoring_type)) %>%
  distinct(scoring_type,season,week,.keep_all = TRUE) %>% 
  ggplot(aes(x = week_num, y = baseline_previous16, color = scoring_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(title = "Rolling 16 game Average of PPG for QB12/Flex84 by scoring type") +
  theme_minimal()

##Calculate FP above the baseline
# game_baselines <- 
#   game_df %>% 
#   inner_join(baseline_prep %>% select(season, week, scoring_type, baseline_previous16, week_num),
#              by = c("season", "week", "scoring_type")) %>% 
#   mutate(fpob = fantasy_points - baseline_previous16)

game_baselines <- 
  game_df %>% 
  inner_join(baseline_prep %>% select(season, week, position, scoring_type, baseline_previous16, week_num),
            by = c("season", "week", "scoring_type", "position")) %>% 
  mutate(fpob = fantasy_points - baseline_previous16)

#Plot fpob by TE finish
game_baselines %>% 
  filter(position == "TE", position_rank <= 32) %>% 
  ggplot(aes(x = position_rank, y = fpob, color = scoring_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) + 
  theme_minimal()

#calculate points above baseline for different TEs
# te_fpob <- 
#   game_baselines %>% 
#   filter(position == "TE") %>% 
#   group_by(scoring_type, position_rank) %>% 
#   summarize(average_fpob = mean(fpob)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = scoring_type,
#               values_from = average_fpob) %>% 
#   mutate(ratio_te2_ppr = te2/ppr,
#          ratio_te15_ppr = te15/ppr,
#          percent_diff_te2_ppr = (te2-ppr) / abs(ppr),
#          percent_diff_te15_ppr = (te15-ppr) / abs(ppr),
#          across(.cols = where(is.numeric),
#                 .fn = ~round(.x, digits = 2)))

overall_fpob <- 
  game_baselines %>% 
  # filter(position == "TE") %>% 
  group_by(scoring_type, position_rank, position) %>% 
  summarize(average_fpob = mean(fpob)) %>%
  ungroup() %>% 
  group_by(scoring_type) %>% 
  mutate(fpob_rank = rank(desc(average_fpob))) %>%
  ungroup() %>% 
  pivot_wider(names_from = scoring_type,
              values_from = c(average_fpob,fpob_rank)) %>% 
  mutate(
    across(tidyselect::contains("fpob_rank"),list(value = ~1000 * exp(.x * -0.021)))
  ) %>% 
  arrange(desc(fpob_rank_ppr_value))

overall_fpob %>% 
  ggplot(aes(x = fpob_rank_ppr_value, y = fpob_rank_te2_value, color = position)) +
  geom_abline(slope = -1, size = 1.5, alpha = 0.5, color = "white") +
  geom_point(alpha = 0.5) + 
  geom_smooth(se = F) + 
  scale_x_reverse() +
  hrbrthemes::theme_modern_rc()


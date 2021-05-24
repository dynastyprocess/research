# Load Packages -----------------------------------------------------------
library(here)
library(tidyverse)
library(arrow)
library(tidymodels)
library(skimr)
library(tidytext)
library(ggridges)
library(GGally)
library(ggthemes)
ggplot2::theme_set(ggthemes::theme_fivethirtyeight())

# Load Data ---------------------------------------------------------------
setwd(here())
pbp <- read_parquet("../ep/data/fit_data/ep_1999_2019.pdata")

yards_created_df <- read_csv("yards_created_data.csv") %>%
  mutate(name = str_to_lower(name)) %>%
  select(name, draft_year, draft_pick, yc_per_attempt, mtf_per_attempt, rec_yard_per_pass_play, rec_share, total_yards_per_team_play)

# RB Data by Season ---------------------------------------------------------------
rb_seasons <- pbp %>%
  filter(season >= 2016, gsis_pos == "RB") %>%
  group_by(season, gsis_id, gsis_name) %>%
  summarise(rec_ypg = mean(rec_yd, na.rm = TRUE),
            rec_total = sum(rec_yd, na.rm = TRUE),
            rush_ypg = mean(rush_yd, na.rm = TRUE),
            rush_total = sum(rush_yd, na.rm = TRUE),
            ppr_ppg = mean(total_fp, na.rm = TRUE),
            ppr_total = sum(total_fp, na.rm = TRUE),
            ep_per_game = mean(total_fp_x, na.rm = TRUE),
            ep_diff_per_game = mean(total_fp_diff, na.rm = TRUE),
            ep_total_diff = sum(total_fp_diff, na.rn = TRUE),
            games = n()) %>%
  ungroup() %>%
  arrange(gsis_id, gsis_name, season) %>%
  group_by(gsis_id, gsis_name) %>%
  mutate(season_number = row_number(),
         gsis_name = str_to_lower(gsis_name)) %>%
  ungroup()

rb_seasons_wide <- rb_seasons %>%
  select(-season) %>%
  pivot_wider(names_from = season_number,
              values_from = c(rec_ypg, rec_total, rush_ypg, rush_total, ppr_ppg, ppr_total, ep_per_game, ep_diff_per_game, ep_total_diff, games),
              names_glue = "{.value}_y{season_number}") %>%
  rowwise() %>%
  mutate(ppr_ppg_total = sum(ppr_total_y1, ppr_total_y2, ppr_total_y3, ppr_total_y4, na.rm = TRUE) /
                         sum(games_y1, games_y2, games_y3, games_y4, na.rm = TRUE),
         rec_ypg_total = sum(rec_total_y1, rec_total_y2, rec_total_y3, rec_total_y4, na.rm = TRUE) /
                         sum(games_y1, games_y2, games_y3, games_y4, na.rm = TRUE),
         rush_ypg_total = sum(rush_total_y1, rush_total_y2, rush_total_y3, rush_total_y4, na.rm = TRUE) /
                         sum(games_y1, games_y2, games_y3, games_y4, na.rm = TRUE),
         ep_diff_per_game_total = sum(ep_total_diff_y1, ep_total_diff_y2, ep_total_diff_y3, ep_total_diff_y4, na.rm = TRUE) /
                         sum(games_y1, games_y2, games_y3, games_y4, na.rm = TRUE)
         ) %>%
  ungroup()

# Correlation plots -------------------------------------------------------
skim(yards_created_df)

yards_created_df %>%
  left_join(rb_seasons_wide, by = c("name" = "gsis_name")) %>%
  transmute(ppr_ppg_total, log(draft_pick), yc_per_attempt, mtf_per_attempt, rec_yard_per_pass_play, rec_share, total_yards_per_team_play) %>%
  ggpairs(lower = list(continuous = wrap("smooth")))

lm1 <- yards_created_df %>%
  left_join(rb_seasons_wide, by = c("name" = "gsis_name")) %>%
  lm(ppr_ppg_total ~ log(draft_pick) + yc_per_attempt + rec_share + total_yards_per_team_play + mtf_per_attempt, data = .)

summary(lm1)

lm2 <- yards_created_df %>%
  left_join(rb_seasons_wide, by = c("name" = "gsis_name")) %>%
  lm(draft_pick ~ yc_per_attempt + rec_share + total_yards_per_team_play + mtf_per_attempt, data = .)

summary(lm2)



# Clustering --------------------------------------------------------------
pca_rec <- recipe(~., data = yards_created_df) %>%
  update_role(name, draft_year, new_role = "id") %>%
  step_log(draft_pick) %>%
  step_medianimpute(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 6)

pca_prep <- prep(pca_rec)

tidied_pca <- tidy(pca_prep, 4)

rb_clusters_wide <- juice(pca_prep) %>%
  pivot_longer(cols = starts_with("PC"), names_to = "Cluster") %>%
  arrange(name, -value) %>%
  group_by(name) %>%
  # filter(row_number() == 1 | row_number() == 2) %>%
  # mutate(rn = row_number()) %>%
  # select(-Cluster) %>%
  # pivot_wider(values_from = c(value), names_from = rn) %>%
  # mutate(`2`-`1`)

  filter(value == max(value)) %>%
  ungroup() %>%
  left_join(rb_seasons_wide, by = c("name" = "gsis_name")) %>%
  left_join(yards_created_df, by = c("name", "draft_year"))

rb_seasons_clusters <- rb_clusters_wide %>%
  select(name, draft_year, Cluster) %>%
  left_join(rb_seasons, by = c("name" = "gsis_name")) %>%
  filter(draft_year < 2020) %>%
  mutate(season = ifelse(is.na(season),draft_year,season)) %>%
  group_by(draft_year, Cluster) %>%
  complete(name, nesting(season)) %>%
  ungroup() %>%
  # arrange(name, season) %>%
  # group_by(name) %>%
  # mutate(season_number = row_number()) %>%
  # ungroup() %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  pivot_longer(cols = where(is.numeric), names_to = "Metric")

skim(rb_clusters_wide)

# summary <- rb_clusters_wide %>%
#   group_by(Cluster) %>%
#   summarise(across(where(is.numeric), mean, na.rm = TRUE),
#             players = n())

rookie_examples <- rb_clusters_wide %>% 
  select(name,draft_year,Cluster) %>% 
  filter(draft_year == 2020) %>% 
  group_by(Cluster) %>% 
  summarise(rookie_examples = paste(name,collapse = ","))

cluster_examples <- rb_clusters_wide %>% 
  select(name, draft_year, Cluster, value) %>% 
  group_by(Cluster) %>% 
  top_n(n=3, wt=value) %>%
  summarise(cluster_examples = paste(name,collapse = ", "))

summarydf <- rb_clusters_wide %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE),
            player_count = n()) %>% 
  select(Cluster,
         ppr_ppg_total,
         rec_ypg_total,
         rush_ypg_total,
         ep_diff_per_game_total,
         player_count,
         draft_pick,
         yc_per_attempt,
         mtf_per_attempt,
         rec_yard_per_pass_play,
         rec_share,
         total_yards_per_team_play) %>% 
  arrange(desc(ppr_ppg_total)) %>% 
  left_join(cluster_examples,by = "Cluster") %>%
  mutate(across(where(is.numeric), ~round(.,2)))




# Visualizations ----------------------------------------------------------

# rb_seasons_clusters %>%
#   filter(Metric %in% c('ppr_ppg', 'rec_ypg', 'ep_diff_per_game', 'games')) %>%
#   ggplot(aes(value, Cluster, fill = factor(stat(quantile)))) + 
#   stat_density_ridges(geom = "density_ridges_gradient", 
#                       calc_ecdf = TRUE,
#                       quantiles = 4,
#                       quantile_lines = TRUE) +
#   facet_wrap(~Metric, scales = "free_x") +
#   scale_fill_viridis_d(name = "Quartiles")

rb_clusters_wide %>%
  select(name, Cluster, draft_pick, yc_per_attempt, mtf_per_attempt, rec_yard_per_pass_play, rec_share, total_yards_per_team_play) %>%
  pivot_longer(cols = where(is.numeric), names_to = "Metric") %>%
  mutate(Metric = factor(Metric, levels = c('draft_pick', 'yc_per_attempt', 'mtf_per_attempt', 'rec_yard_per_pass_play',
                                            "rec_share", "total_yards_per_team_play"))) %>%
  ggplot(aes(value, Cluster, fill = 100*stat(ecdf))) + 
  stat_density_ridges(geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = 4,
                      quantile_lines = TRUE,
                      jittered_points = TRUE,
                      position = "points_sina",
                      alpha = 0.8,
                      point_color = "black"
  ) + 
  labs(x = NULL,
       y = NULL,
       fill = "Percentile") +
  facet_wrap(~Metric,
             scales = "free_x",
             nrow = 3,
             labeller = as_labeller(c('draft_pick' = 'Overall Draft Pick',
                                      'yc_per_attempt' = 'Yards Created per attempt',
                                      'mtf_per_attempt' = 'Missed Tackles Forced per attempt',
                                      'rec_yard_per_pass_play' = 'Receiving Yards per pass play',
                                      'rec_share' = 'Reception Share',
                                      'total_yards_per_team_play' = 'Total Yards per team play')))


rb_seasons_clusters %>%
  filter(Metric %in% c('ppr_ppg', 'rec_ypg', 'ep_diff_per_game', 'games')) %>%
  mutate(Metric = factor(Metric, levels = c('ppr_ppg', 'rec_ypg', 'ep_diff_per_game', 'games'))) %>%
  ggplot(aes(value, Cluster, fill = 100*stat(ecdf))) + 
    stat_density_ridges(geom = "density_ridges_gradient",
                        calc_ecdf = TRUE,
                        quantiles = 4,
                        quantile_lines = TRUE,
                        jittered_points = TRUE,
                        position = "points_sina",
                        alpha = 0.8,
                        point_color = "black"
                        ) + 
  labs(x = NULL,
       y = NULL,
       fill = "Percentile") +
  facet_wrap(~Metric,
             scales = "free_x",
             labeller = as_labeller(c('ppr_ppg' = 'PPR Points Per Game',
                                      'rec_ypg' = 'Receiving Yards Per Game',
                                      'ep_diff_per_game' = 'Fantasy Points Over Expected Per Game',
                                      'games' = 'Games Played')))
  

tidied_pca %>%
  filter(component %in% paste0("PC", 1:6)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:6)) %>%
  group_by(component) %>%
  top_n(5, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Variable Importance by Tier",
    y = NULL,
    fill = "Positive Contribution?"
  )


juice(pca_prep) %>%
  ggplot(aes(PC2, PC5, label = name)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward") +
  labs(color = NULL)

sdev <- pca_prep$steps[[4]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

tibble(
  component = unique(tidied_pca$component),
  percent_var = percent_variation ## use cumsum() to find cumulative, if you prefer
) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_var)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "Percent variance explained by each PCA component")
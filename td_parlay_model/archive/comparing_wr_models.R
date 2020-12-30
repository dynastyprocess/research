suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(slider)
  library(lubridate)
  
  # Plotting
  library(ggbeeswarm)
  library(ggthemes)
  library(directlabels)
  library(ggrepel)
  
  #EDA
  library(skimr)
  library(usemodels)
  library(tidymodels)
  library(vip)
  library(baguette)
})

# Import Data -------------------------------------------------------------
setwd(here::here())

load("parlay_pred_models_updated.rda")


ep_feature_mart <- read_arrow("C:/Users/syd23/Documents/DynastyProcess/db/feature_mart.pdata")

td_model_prep <- ep_feature_mart %>%
  filter(Season == 2020, Pos %in% c("WR")) %>% 
  select(-(contains("ecr") & !(contains("combo") | contains("teammate")))) %>% 
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id) %>%
  mutate(across(.cols = c(parlay_td, posteam_type, spread_line, total_line, implied_total,
                          ecr_ovr_combo, ecr_pos_combo,
                          teammate_ecr_rank, teammate_ecr_gap_to_better,
                          teammate_ecr_gap_to_next, teammate_ecr_gap_to_best),
                .fns = ~lead(.x), 
                .names = "{.col}_next")) %>% 
  ungroup() %>% 
  filter(!is.na(posteam_type_next)) %>% 
  bind_cols(predict(wr_td_pred, .)) %>% 
  select(Name, Week, Team, ecr_pos_combo_next, ecr_pos_combo, ecr_ovr_combo_next, ecr_ovr_combo, parlay_td_next,
         total_fp_share_roll10, .pred) %>% 
  filter(.pred>0.8)

td_model_prep %>% 
  filter(.pred > 0) %>% 
  rmse(parlay_td_next, .pred)

td_model_prep %>% 
  filter(.pred > 0) %>% 
  rsq(parlay_td_next, .pred)

# names(which(colSums(is.na(td_model_prep))>0))
# 
# names(td_model_prep %>% select(!where(is.numeric)))
# 
# td_model_prep %>% group_by(Season, Week, posteam_type_next) %>% tally()
# 
# ep_feature_mart %>% filter(Season == "2020") %>% select(Week) %>% distinct()
# 
# temp <- td_model_prep %>% filter(is.na(posteam_type_next))


load("parlay_pred_models.rda")
ep_lagged_lines <- read_arrow("model_roll.pdata")

td_model_prep <- ep_lagged_lines %>%
  filter(Season == 2020, Pos == "WR", !is.na(posteam_type_next)) %>%
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id) %>%
  mutate(across(.cols = c(ecr_ovr, ecr_pos),
                .fns = ~lead(.x), 
                .names = "{.col}_pred_next")) %>% 
  ungroup() %>% 
  bind_cols(predict(wr_td_pred, .))

td_model_prep %>% 
  filter(.pred > 0) %>% 
  rmse(parlay_td_next, .pred)

td_model_prep %>% 
  filter(.pred > 0) %>% 
  rsq(parlay_td_next, .pred)

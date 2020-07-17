library(tidyverse)
library(furrr)
library(arrow)

plan(multisession)
options(dplyr.summarise.inform = FALSE)

sfb_picks <- read_parquet("data/sfb_picks.pdata")
sfb_teams <- unique(sfb_picks$franchise_name)

calculate_playersims <- function(sfb_picks,user){

  user_picks <- sfb_picks %>%
    filter(franchise_name == user & !is.na(player_name))

  sfb_picks %>%
    mutate(sim_score = 1000*exp(-0.03 * pos_adp)) %>%
    select(division_name,franchise_name,
           player_id,player_name,pos,age,team,
           pos_adp,sim_score) %>%
    semi_join(user_picks,by = c('player_id')) %>%
    group_by(franchise_name) %>%
    summarise(total_sim_score = sum(sim_score,na.rm=TRUE),
              total_sim_score = round(total_sim_score),
              matching_count = n(),
              matching_players = paste(player_name,collapse = "; ")) %>%
    arrange(desc(total_sim_score)) %>%
    slice(-1)

}

top_five_playersims <- function(user,sfb_picks){
  calculate_playersims(sfb_picks,user) %>%
    slice(1:5)
}

player_sims <- tibble(sfb_teams = sfb_teams) %>%
  mutate(sfb_sims = future_map(sfb_teams,top_five_playersims,sfb_picks)) %>%
  unnest(sfb_sims) %>%
  mutate(scaled_simscores = scale(total_sim_score)) %>%
  group_by(sfb_teams) %>%
  summarise(mean_sim_score = mean(scaled_simscores)) %>%
  arrange(mean_sim_score) %>%
  mutate(rank = rank(mean_sim_score,ties.method = "random"))

strategic_sims <- pca_dist %>%
  select(franchise_name,contains("kentweyrauch"))

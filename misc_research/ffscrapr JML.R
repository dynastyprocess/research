library(ffscrapr)
library(tidyverse)

solarpool_leagues <- sleeper_userleagues("solarpool",2020)

jml_id <- solarpool_leagues %>% 
  filter(league_name == "The JanMichaelLarkin Dynasty League") %>% 
  pull(league_id)


jml <- sleeper_connect(season = 2020, league_id = jml_id)

jml_rosters <- ff_rosters(jml)

player_values <- dp_values("values-players.csv")

player_ids <- dp_playerids() %>% 
  select(sleeper_id,fantasypros_id)

player_values <- player_values %>% 
  left_join(player_ids, by = c("fp_id" = "fantasypros_id")) %>% 
  select(sleeper_id,ecr_1qb,ecr_pos,value_1qb)

jml_values <- jml_rosters %>% 
  left_join(player_values, by = c("player_id"="sleeper_id")) %>% 
  arrange(franchise_id,desc(value_1qb))

trade <- jml_values %>% 
  filter(franchise_name %in% c("sox05syd","Fake News"))
# Libraries -------------------------------------------------------------

library(arrow)
library(tidyverse)
library(here)
library(corrr)
library(slider)

setwd(here::here())

# Functions -------------------------------------------------------------
get_rate <- function(x,y){
  rate <- sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE)
  
  ifelse(is.nan(rate) | is.infinite(rate), 0, rate)
}

# Rushing -------------------------------------------------------------
df_rushing <- 
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/osdb/rushing") %>% 
  dplyr::collect() %>% 
  mutate(rush_yards_before_contact = rushing_attempts * rush_yards_before_contact_average,
         rush_yards_after_contact = rushing_attempts * yards_after_contact_per_rush,
         rushing_attempts_stuffed = rushing_attempts * rushing_stuff_pct / 100)

#Which Years do we have broken tackles?
df_rushing %>% filter(!is.na(broken_tackles)) %>% group_by(season) %>% tally()
df_rushing %>% filter(!is.na(yards_after_contact_per_rush)) %>% group_by(season) %>% tally()

#evaluate O-lines
rushing_year <- 
  df_rushing %>%
  arrange(team_by_historic_name, season, week) %>%
  group_by(season, week, team_by_historic_name) %>%
  summarise(
    across(.cols = c(rushing_yards,
                     rushing_attempts,
                     broken_tackles,
                     rush_yards_before_contact,
                     rush_yards_after_contact,
                     rushing_attempts_stuffed),
           .fns = ~sum(.x, na.rm = TRUE)),
    rush_stuff_rate_game = rushing_attempts_stuffed / rushing_attempts,
    rush_ybc_rate_game = rush_yards_before_contact / rushing_yards,
    rush_yac_rate_game = rush_yards_after_contact / rushing_yards,
    rush_ypc_game = rushing_yards / rushing_attempts
    ) %>% 
  ungroup() %>% 
  group_by(season, team_by_historic_name) %>%
  mutate(game_num = row_number(),
         rush_stuff_rate_season = slide2_dbl(rushing_attempts_stuffed, rushing_attempts, ~get_rate(.x,.y), .before = Inf, .after = -1),
         rush_ybc_rate_season = slide2_dbl(rush_yards_before_contact, rushing_yards, ~get_rate(.x,.y), .before = Inf, .after = -1),
         rush_yac_rate_season = slide2_dbl(rush_yards_after_contact, rushing_yards, ~get_rate(.x,.y), .before = Inf, .after = -1),
         rush_ypc_season = slide2_dbl(rushing_yards, rushing_attempts, ~get_rate(.x,.y), .before = Inf, .after = -1)) %>% 
  ungroup()

rushing_year %>% 
  filter(season >= 2007) %>% 
  ggplot(aes(x = rush_yac_rate_game, y = rush_yac_rate_season)) +
  geom_point() +
  geom_smooth() +
  xlim(0,1) +
  ylim(0,1) +
  theme_minimal()

#Check correlations
corrr::correlate(df_rushing %>% select(where(is.numeric))) %>% focus(rushing_yards)
corrr::correlate(rushing_year %>% select(where(is.numeric))) %>% focus(rush_stuff_rate_game)

correlate(x = rushing_year$rush_stuff_rate_season, y = rushing_year$rush_stuff_rate_game)
correlate(x = rushing_year$rush_ybc_rate_season, y = rushing_year$rush_ybc_rate_game)
correlate(x = rushing_year$rush_yac_rate_season, y = rushing_year$rush_yac_rate_game)
correlate(x = rushing_year$rush_ypc_season, y = rushing_year$rush_ypc_game)

correlate(x = rushing_year %>% filter(week == 5) %>% pull(rush_yac_rate_season),
          y = rushing_year %>% filter(week == 5) %>% pull(rush_yac_rate_game))

rushing_year %>% 
  group_by(game_num) %>% 
  summarise(correlation_coef = cor(rush_ypc_season, rush_ypc_game),
            n()) %>% 
  ungroup() %>%
  ggplot(aes(x = game_num, y = correlation_coef)) +
    geom_point() +
    geom_smooth() +
    theme_minimal()


rushing_year %>% 
  group_by(week, season) %>% 
  tally() %>% 
  view()


#Testing regressions
lm(rushing_yards ~ rushing_attempts + rushing_stuff_pct, data = df_rushing) %>% summary()
lm(rushing_yards ~ rushing_attempts + broken_tackles, data = df_rushing) %>% summary()
lm(rushing_yards ~ rushing_attempts + yards_after_contact_per_rush, data = df_rushing) %>% summary()


#Rec
df_receiving <- 
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/osdb/receiving") %>% # dataset of choice
  filter(season > 2000) %>% # some filters can be passed in before collect
  dplyr::collect() # actually puts df in your memory
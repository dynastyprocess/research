library(arrow)
library(tidyverse)
library(here)
library(corrr)

setwd(here::here())

df_rushing <- 
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/osdb/rushing") %>% # dataset of choice
  filter(season > 2000) %>% # some filters can be passed in before collect
  dplyr::collect() %>% # actually puts df in your memory
  mutate(rush_yards_before_contact = rushing_attempts * rush_yards_before_contact_average,
         rushing_attempts_stuffed = rushing_attempts * rushing_stuff_pct / 100)

#Which Years do we have broken tackles?
df_rushing %>% filter(!is.na(broken_tackles)) %>% group_by(season) %>% tally()
df_rushing %>% filter(!is.na(yards_after_contact_per_rush)) %>% group_by(season) %>% tally()

#evaluate O-lines
rushing_year <- 
  df_rushing %>% 
  group_by(season, team_by_historic_name) %>%
  summarise(across(.cols = c(rushing_yards, rushing_attempts, broken_tackles,
                             rush_yards_before_contact, rushing_attempts_stuffed),
                   .fns = ~sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(yards_per_carry = rushing_yards / rushing_attempts,
         yards_before_contact_pct = rush_yards_before_contact / rushing_yards)

rushing_year %>% 
  filter(season >= 2007) %>% 
  ggplot(aes(x = yards_before_contact_pct, y = yards_per_carry)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

#Check correlations
corrr::correlate(df_rushing %>% select(where(is.numeric))) %>% focus(rushing_yards)
corrr::correlate(rushing_year %>% select(where(is.numeric))) %>% focus(yards_per_carry)


#Testing regressions
lm(rushing_yards ~ rushing_attempts + rushing_stuff_pct, data = df_rushing) %>% summary()
lm(rushing_yards ~ rushing_attempts + broken_tackles, data = df_rushing) %>% summary()
lm(rushing_yards ~ rushing_attempts + yards_after_contact_per_rush, data = df_rushing) %>% summary()


#Rec
df_receiving <- 
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/osdb/receiving") %>% # dataset of choice
  filter(season > 2000) %>% # some filters can be passed in before collect
  dplyr::collect() # actually puts df in your memory
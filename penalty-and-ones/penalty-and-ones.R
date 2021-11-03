library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)
library(here)

setwd(here())

pbp <- 
  load_pbp(2011:2021) %>% 
  mutate(desc_cleaned = str_to_lower(desc))

rosters <- 
  load_rosters(2011:2021) %>% 
  select(season, gsis_id, full_name)

pbp %>% 
  filter(str_detect(desc_cleaned, "declined"),
         str_detect(desc_cleaned, "defensive pass interference"),
         !is.na(receiving_yards)) %>%
  left_join(rosters, by = c("season", "receiver_player_id" = "gsis_id")) %>% 
  group_by(receiver_player_id) %>% 
  summarize(full_name = last(full_name),
            plays = n(),
            across(.cols = c(receiving_yards, yards_after_catch, pass_touchdown, epa, vegas_wpa),
                   .fns = sum)) %>% 
  ungroup() %>% 
  select(-receiver_player_id) %>% 
  arrange(-plays, -epa) %>% 
  slice_head(n = 20) %>% 
  gt()%>%
  gt_theme_espn() %>% 
  tab_header(title = 'Football "And-One" Leaderboard: 2011-2021',
             subtitle = "Plays where the defender commits pass interference and the receiver still catches it") %>%
  cols_label(full_name = "Player",
             plays = "Plays",
             receiving_yards = "Yards",
             yards_after_catch = "Yards After Catch",
             pass_touchdown = "Touchdowns",
             epa = "Expected Points Added",
             vegas_wpa = "Win Probability Added") %>%
  fmt_percent(columns = vegas_wpa, decimals = 1) %>% 
  fmt_number(columns = epa, decimals = 1) %>%
  tab_footnote(footnote = 'h/t Robert Mays and Benjamin Solak - The Athletic Football Show',
               locations = cells_title(groups = c("subtitle"))) %>% 
  tab_source_note(source_note = "Chart: @JoeSydlowskiFF | Data: nflfastR") %>% 
  gtsave(filename = "and-one-chart.png")

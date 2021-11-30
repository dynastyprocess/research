library(tidyverse)
library(nflreadr)
library(gt)
library(gtExtras)
library(here)

setwd(here())

pbp <- 
  load_pbp(1999:2021) %>% 
  mutate(desc_cleaned = str_to_lower(desc))

rosters <- 
  load_rosters(1999:2021) %>% 
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
  arrange(-plays, -epa) %>% view()
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

# pbp %>% 
#   filter(str_detect(desc_cleaned, "declined"),
#          str_detect(desc_cleaned, "defensive pass interference"),
#          receiver_player_name == "J.Jones",
#          !is.na(receiving_yards))%>% 
#   transmute(receiver_player_name, season, posteam, week, desc, yardline_100, air_yards, targetline = yardline_100 - air_yards) %>% 
#   view()

  pbp %>% 
    filter(str_detect(desc_cleaned, "illegal contact"),
           !is.na(receiver_id)) %>%
    left_join(rosters, by = c("season", "receiver_id" = "gsis_id"), na_matches = "never") %>%
    # select(desc, full_name, contains("penalty"), contains("receiver"), contains("yard")) %>% view()
    
    group_by(receiver_id) %>% 
    summarize(full_name = last(full_name),
              plays = n(),
              across(.cols = c(receiving_yards, yards_after_catch, penalty_yards, pass_touchdown, epa, vegas_wpa),
                     .fns = ~sum(.x, na.rm = TRUE))) %>% 
    ungroup() %>% 
    select(-receiver_id) %>% 
    arrange(-plays, -epa) %>%
    slice_head(n = 20) %>% 
    gt()%>%
    gt_theme_espn() %>% 
    tab_header(title = 'DPIs Drawn: 1999-2021',
               subtitle = "Penalties accepted and declined") %>%
    cols_label(full_name = "Player",
               plays = "Plays",
               receiving_yards = "Yards",
               yards_after_catch = "Yards After Catch",
               penalty_yards = "Penalty Yards",
               pass_touchdown = "Touchdowns",
               epa = "Expected Points Added",
               vegas_wpa = "Win Probability Added") %>%
    fmt_percent(columns = vegas_wpa, decimals = 1) %>% 
    fmt_number(columns = epa, decimals = 1) %>%
    # tab_footnote(footnote = 'h/t Robert Mays and Benjamin Solak - The Athletic Football Show',
    #              locations = cells_title(groups = c("subtitle"))) %>% 
    tab_source_note(source_note = "Chart: @JoeSydlowskiFF | Data: nflfastR") %>% 
    gtsave(filename = "dpis-drawn.png")
  
  
  pbp %>% 
    filter(penalty_type == "Defensive Pass Interference") %>% 
    slice_sample(n = 20) %>% 
    select(desc, contains("receiver"), contains("yard")) %>% view()
  
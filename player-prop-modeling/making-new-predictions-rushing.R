# Libraries
library(tidyverse)
library(nflreadr)
library(here)
library(odds.converter)
library(furrr)
library(slider)

setwd(here())
plan(multisession, workers = 3)

current_week <- 14

get_rate <- function(x, y){
  rate <- sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE)
  
  ifelse(is.nan(rate) | is.infinite(rate), 0, rate)
}

year_vector <- 2020:2022

# Fantasy Pros Positional Ranks
fantasy_pros_data <- 
  readRDS("data/fantasy_pros_weekly_2012_2021.RDS") %>%
  bind_rows(readRDS("data/fantasy_pros_weekly_2022.RDS")) %>%
  # ffpros::fp_rankings(page = "ppr-flex", sport = "nfl") %>% 
  # bind_rows(ffpros::fp_rankings(page = "qb", sport = "nfl")) %>% 
  mutate(merge_name = clean_player_names(player_name)) %>%
  filter((pos %in% c("WR","RB","TE") | page_pos == "QB")) %>% 
  transmute(
    merge_name,
    season,
    week,
    position = pos,
    rank,
    sd,
    team = if_else(season == 2022 & week == current_week, team, NA_character_),
    team = clean_team_abbrs(team)
    
  )
         
# Load and clean data
expected_points_data <- 
  # TODO Missing 2021 Superbowl
  nflreadr::load_ff_opportunity(seasons = year_vector) %>%
  filter(position %in% c("QB","WR","RB","TE"),
         # Bad Steve Smith
         player_id != "00-0025438") %>% 
  mutate(merge_name = clean_player_names(full_name),
         merge_name = case_when(merge_name == "Charles D Johnson" ~ "Charles Johnson",
                                merge_name == "Bisi Johnson" ~ "Olabisi Johnson",
                                merge_name == "William Fuller" ~ "Will Fuller",
                                TRUE ~ merge_name),
         position = case_when(merge_name == "JD McKissic" ~ "RB",
                              merge_name == "Ty Montgomery" ~ "RB",
                              merge_name == "Dexter McCluster" ~ "RB",
                              merge_name == "Lynn Bowden" ~ "RB",
                              TRUE ~ position),
         season = as.numeric(season),
         results_team = clean_team_abbrs(posteam)) %>% 
  select(season, week, results_team, position, merge_name, contains("rush") & !contains("two_point"))

already_played <- expected_points_data %>%
  filter(season == 2022, week == current_week) %>% 
  distinct(results_team) %>% 
  pull()

expected_points_new_week <- 
  tibble(season = 2022,
         week = current_week,
         results_team = expected_points_data %>% distinct(results_team) %>% pull()) %>% 
  filter(results_team %ni% already_played)

team_receiving_trends <- 
  expected_points_data %>% 
  bind_rows(expected_points_new_week) %>% 
  select(season, week, team = results_team, contains("rush") & contains("team")) %>% 
  distinct() %>% 
  arrange(season, week) %>% 
  group_by(team) %>% 
  mutate(
    across(.cols = where(is.numeric) & !contains("season") & !contains("week"),
           .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
           .names = "{.col}_roll4"), 
  ) %>%
  ungroup()
  # select(season, week, team, contains("roll"))

# Roster data to find player teams by week
weekly_rosters <-
  nflreadr::load_rosters_weekly(seasons = year_vector) %>%
  filter(position %in% c("QB","WR","RB","TE"),
         # Bad Steve Smith
         !(full_name == "Steve Smith" & team == "SL")) %>%
  mutate(merge_name = clean_player_names(full_name),
         merge_name = case_when(merge_name == "Charles D Johnson" ~ "Charles Johnson",
                                merge_name == "Bisi Johnson" ~ "Olabisi Johnson",
                                merge_name == "William Fuller" ~ "Will Fuller",
                                TRUE ~ merge_name),
         position = case_when(merge_name == "JD McKissic" ~ "RB",
                              merge_name == "Ty Montgomery" ~ "RB",
                              merge_name == "Dexter McCluster" ~ "RB",
                              merge_name == "Lynn Bowden" ~ "RB",
                              TRUE ~ position)
  ) %>%
  distinct(season, week, position, rosters_team = clean_team_abbrs(team), merge_name)

# Snap Data
snap_data <- nflreadr::load_snap_counts(seasons = year_vector) %>%
  transmute(season,
            week,
            merge_name = clean_player_names(player),
            merge_name = case_when(merge_name == "Charles D Johnson" ~ "Charles Johnson",
                                   merge_name == "Bisi Johnson" ~ "Olabisi Johnson",
                                   merge_name == "William Fuller" ~ "Will Fuller",
                                   merge_name == "Ben Watson" ~ "Benjamin Watson",
                                   merge_name == "Tim Wright" ~ "Timothy Wright",
                                   merge_name == "TJ Graham" ~ "Trevor Graham",
                                   merge_name == "Jonathan Baldwin" ~ "Jon Baldwin",
                                   TRUE ~ merge_name),
            position = case_when(merge_name == "JD McKissic" ~ "RB",
                                 merge_name == "Ty Montgomery" ~ "RB",
                                 merge_name == "Dexter McCluster" ~ "RB",
                                 merge_name == "Lynn Bowden" ~ "RB",
                                 merge_name == "Jesper Horsted" ~ "TE",
                                 merge_name == "Juwan Johnson" ~ "TE",
                                 merge_name == "Dan Arnold" ~ "TE",
                                 merge_name == "Neal Sterling" ~ "TE",
                                 merge_name == "Jamal Agnew" ~ "WR",
                                 merge_name == "Equanimeous St Brown" ~ "WR",
                                 position %in% c("FB","HB","FB/D","FB/R","FB/T","RB/F","RB/W") ~ "RB",
                                 position %in% c("TE/D") ~ "TE",
                                 position %in% c("WR/R") ~ "WR",
                                 TRUE ~ position),
            snap_team = clean_team_abbrs(team),
            player_snaps = offense_snaps) %>% 
  filter(position %in% c("QB","WR","RB","TE"))


max_snaps <- 
  snap_data %>% 
  group_by(season, week, team = snap_team) %>% 
  # 16 games where no one played every offensive snap
  slice_max(player_snaps, n = 1) %>% 
  summarise(team_snaps = max(player_snaps)) %>% 
  ungroup()

# Injury Report Data

clean_injury_field <- function(injury_field) {
  
  case_when(
    str_detect(injury_field, "(Not injury related)|rest|Rest|(Load Management)|(Not Injury Related)|Travel") |
      is.na(injury_field) ~
      "No Injuries",
    str_detect(injury_field, "hand|Hand|finger|Finger|thumb|Thumb|Wrist") ~ "Hand/Wrist",
    str_detect(injury_field, "Illness|COVID|Covid") ~ "Illness",
    str_detect(
      injury_field, 
      "Knee|knee|Hamstring|Calf|Thigh|Quad|Groin|groin|Hip|Achilles|Shin|Fibula|Glute|Butt|Adductor|calve|calf|Leg|leg|Pelvis") ~
      "Leg/Hip",
    
    str_detect(injury_field, "Ankle|ankle|Foot|Toe|Heel|Feet") ~ "Foot/Ankle",
    str_detect(injury_field, "Back|Rib|Abdomen|Oblique|Core|Stomach") ~ "Stomach/Back/Rib",
    str_detect(injury_field, "Neck|Concussion|Head") ~ "Head/Neck",
    str_detect(injury_field, 
               "Arm|arm|Elbow|Chest|Shoulder|shoulder|Pectoral|Bicep|Tricep|Collarbone|Clavicle|tricep") ~
      "Arm/Chest",
    TRUE ~ "Other")
  
}

injury_data <- nflreadr::load_injuries(seasons = year_vector) %>% 
  filter(position %in% c("QB","WR","RB","TE"),
         !(full_name == "Martellus Bennett" & week == 10 & season == 2017 & team == "GB")) %>% 
  mutate(merge_name = clean_player_names(full_name),
         merge_name = case_when(merge_name == "Charles D Johnson" ~ "Charles Johnson",
                                merge_name == "Bisi Johnson" ~ "Olabisi Johnson",
                                merge_name == "William Fuller" ~ "Will Fuller",
                                TRUE ~ merge_name),
         position = case_when(merge_name == "JD McKissic" ~ "RB",
                              merge_name == "Ty Montgomery" ~ "RB",
                              merge_name == "Dexter McCluster" ~ "RB",
                              merge_name == "Lynn Bowden" ~ "RB",
                              TRUE ~ position),
         injury_team = clean_team_abbrs(team),
         report_primary_injury_cleaned = clean_injury_field(report_primary_injury),
         report_secondary_injury_cleaned = clean_injury_field(report_secondary_injury),
         practice_primary_injury_cleaned = clean_injury_field(practice_primary_injury),
         practice_secondary_injury_cleaned = clean_injury_field(practice_secondary_injury)
         
  )

# injury_data %>%
#   group_by(practice_secondary_injury, practice_secondary_injury_cleaned) %>%
#   tally() %>%
#   view()
# 
# 
# injury_data %>%
#   group_by(report_primary_injury, practice_primary_injury) %>%
#   tally() %>%
#   view()

# Vegas Line Data
remove_vig_power <- function(away_prob, home_prob, overround_limit = 1e-5, verbose = FALSE){
  probs <- c(away_prob, home_prob)
  # see http://dx.doi.org/10.11648/j.ajss.20170506.12
  n <- length(probs)
  pi <- sum(probs) # booksum
  error <- abs(pi-1) # overround
  # to check how many iterations were necessary
  if(isTRUE(verbose)) cli::cli_alert_info("overround: {.val {error}}")
  if(error <= overround_limit) return(probs)
  k <- log(n) / log(n / pi)
  new_probs <- probs ^ k
  remove_vig_power(new_probs[1], new_probs[2], overround_limit = overround_limit, verbose = verbose)
}

closing_lines <- nflreadr::load_schedules(seasons = year_vector) %>%
  # One game missing ML
  filter(!is.na(away_moneyline)) %>% 
  mutate(away_moneyline = if_else(game_id == "2017_04_CHI_GB", 285L, away_moneyline),
         home_moneyline = if_else(game_id == "2017_04_CHI_GB", -285L, home_moneyline),
         away_win_prob = odds.converter::odds.us2prob(away_moneyline),
         home_win_prob = odds.converter::odds.us2prob(home_moneyline),
         win_probs_unvig = future_map2(away_win_prob, home_win_prob, remove_vig_power,
                                       .progress = TRUE)
  ) %>% 
  unnest_wider(win_probs_unvig, names_sep = "_")

games_away <- closing_lines %>%
  transmute(
    week,
    season,
    game_id,
    team = clean_team_abbrs(away_team),
    home_away = "away",
    win_probability = win_probs_unvig_1,
    game_total = total_line,
    team_total = if_else(
      spread_line <= 0,
      (total_line + spread_line) / 2 - spread_line,
      (total_line - spread_line) / 2
    )
  )

games_home <- closing_lines %>%
  transmute(
    week,
    season,
    game_id,
    team = clean_team_abbrs(home_team),
    home_away = "home",
    win_probability = win_probs_unvig_2,
    game_total = total_line,
    team_total = if_else(
      spread_line >= 0,
      (total_line - spread_line) / 2 + spread_line,
      (total_line + spread_line) / 2
    )
  )

closing_lines_final <-
  games_away %>%
  bind_rows(games_home)

# Combine data into one table
player_game_weeks <- 
  expected_points_data %>% 
  
  # We want the team stats from "team_receiving_trends" for players that got snaps but not usage
  select(dplyr::everything(), -contains("team"), results_team) %>% 
  
  full_join(snap_data, by = c("season", "week", "merge_name", "position")) %>% 
  full_join(injury_data, by = c("season", "week", "merge_name", "position")) %>% 
  mutate(team = coalesce(results_team, snap_team, injury_team)) %>% 
  
  select(season,
         week,
         position,
         team,
         merge_name,
         
         player_snaps,
         
         report_status,
         report_primary_injury_cleaned,
         report_secondary_injury_cleaned,
         
         practice_status,
         practice_primary_injury_cleaned,
         practice_secondary_injury_cleaned,
         
         contains("rush"))

joined_weekly_df <- 
  fantasy_pros_data %>% 
  full_join(player_game_weeks, by = c("season", "week", "merge_name", "position")) %>% 
  mutate(team = coalesce(team.x, team.y)) %>%
  
  left_join(weekly_rosters, by = c("season", "week", "merge_name", "position")) %>%
  mutate(team = coalesce(team, rosters_team)) %>%
  # Fill in missing teams with data from the same season
  # group_by(merge_name, season, position) %>%
  # fill(team, .direction = "updown") %>% 
  # ungroup() %>% 
  filter(!is.na(team)) %>% 
  
  # Join these after getting the final teams from rosters for ranked players
  left_join(closing_lines_final, by = c("season", "week", "team")) %>%
  left_join(team_receiving_trends, by = c("season", "week", "team")) %>%
  left_join(max_snaps, by = c("season", "week", "team" = "team")) %>% 
  
  # Clean injury reports now that we have rankings and results ,Fill in NAs
  mutate(
    
    report_primary_injury_cleaned = replace_na(report_primary_injury_cleaned, "No Injuries"),
    report_secondary_injury_cleaned = replace_na(report_secondary_injury_cleaned, "No Injuries"),
    report_injury_count = as.numeric(report_primary_injury_cleaned != "No Injuries") +
      as.numeric(report_secondary_injury_cleaned != "No Injuries"),
    report_status = replace_na(report_status, "No Injury Status"),
    
    practice_primary_injury_cleaned = replace_na(practice_primary_injury_cleaned, "No Injuries"),
    practice_secondary_injury_cleaned = replace_na(practice_secondary_injury_cleaned, "No Injuries"),
    practice_injury_count = as.numeric(practice_primary_injury_cleaned != "No Injuries") +
      as.numeric(practice_secondary_injury_cleaned != "No Injuries"),
    practice_status = replace_na(practice_status, "No Injury Status"),
    practice_status = if_else(str_trim(practice_status) == "", "No Injury Status", practice_status),
    
    rank = replace_na(rank, 410),
    across(.cols = where(is.numeric),
           .fns = ~replace_na(.x, 0))
    
  ) %>% 
  
  arrange(merge_name, position, season, week) %>% 
  
  group_by(merge_name, position, season) %>% 
  
  mutate(
    across(.cols = c(report_injury_count, practice_injury_count),
           .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
           .names = "{.col}_roll4"),
    last_week_report_status = lag(report_status, default = "No Injury Status"),
    last_week_practice_status = lag(practice_status, default = "No Injury Status")
  ) %>% 
  ungroup() %>%
  
  # No Rankings for playoff games, only want players who played
  filter(week <= 17, player_snaps > 0 | (season == 2022 & week == current_week)) %>% 
  
  # Populate QB1, RB1, RB2, ect. columns
  group_by(team,
           season,
           week,
           postion_group = case_when(position %in% c('WR','TE') ~ 'Rec',
                                     TRUE ~ position)) %>% 
  arrange(rank) %>% 
  mutate(team_position_rank = row_number(),
         position_rank = glue::glue("{postion_group}{team_position_rank}"),
         rank_copy = rank) %>%
  ungroup() %>%
  
  filter(position_rank %in% c("QB1", "RB1", "RB2", "RB3", 
                              "Rec1", "Rec2", "Rec3", "Rec4", "Rec5", "Rec6")) %>% 
  
  pivot_wider(
    names_from = position_rank,
    values_from = rank_copy
  ) %>% 
  # Fill in ranks for the whole team
  group_by(season, week, team) %>%
  fill(c(QB1, RB1, RB2, RB3, Rec1, Rec2, Rec3, Rec4, Rec5, Rec6), .direction = "updown") %>% 
  ungroup() %>% 
  
  # TODO grab teammate Rec positions
  mutate(
    across(.cols = c(QB1, RB1, RB2, RB3, Rec1, Rec2, Rec3, Rec4, Rec5, Rec6),
           .fns = ~replace_na(.x, 410)),
    
    position_rank = glue::glue("{postion_group}{team_position_rank}"),
    teammate_qb_rank = QB1,
    teammate_best_rec_rank = if_else(position_rank == "Rec1", Rec2, Rec1),
    teammate_2nd_best_rec_rank = if_else(position_rank %in% c("Rec1","Rec2"), Rec3, Rec2),
    teammate_3rd_best_rec_rank = if_else(position_rank %in% c("Rec1","Rec2","Rec3"), Rec4, Rec3),
    teammate_4th_best_rec_rank = if_else(position_rank %in% c("Rec1","Rec2","Rec3","Rec4"), Rec5, Rec4),
    teammate_5th_best_rec_rank = if_else(position_rank %in% c("Rec1","Rec2","Rec3","Rec4","Rec5"), Rec6, Rec5),
    # teammate_6th_best_rec_rank = if_else(position_rank %in% c("Rec1","Rec2","Rec3","Rec4","Rec5","Rec6"), Rec7, Rec6),
    
    teammate_best_rb_rank = if_else(position_rank == "RB1", RB2, RB1),
    teammate_2nd_best_rb_rank = if_else(position_rank %in% c("RB1","RB2"), RB3, RB2),
    # teammate_3rd_best_rb_rank = if_else(position_rank %in% c("RB1","RB2","RB3"), RB4, RB3),
  ) %>% 
  select(-c(QB1, RB1, RB2, RB3, Rec1, Rec2, Rec3, Rec4, Rec5, Rec6)) %>% 
  group_by(season,
           week,
           team,
           postion_group = case_when(position %in% c('WR','TE') ~ 'Rec',
                                     TRUE ~ position)) %>%
  mutate(
    teammate_gap_to_best_rank = rank[1] - rank,
    teammate_gap_to_2nd_best_rank = rank[2] - rank,
    teammate_gap_to_3rd_best_rank = rank[3] - rank,
    
    teammate_gap_to_2nd_best_rank = if_else(is.na(teammate_gap_to_2nd_best_rank), 410 - rank, teammate_gap_to_2nd_best_rank),
    teammate_gap_to_3rd_best_rank = if_else(is.na(teammate_gap_to_3rd_best_rank), 410 - rank, teammate_gap_to_3rd_best_rank),
    
    teammate_gap_to_next_best_rank = lead(rank, default = 0) - rank
    
  ) %>% 
  ungroup() %>% 
  
  arrange(season, week) %>% 
  group_by(merge_name, position) %>% 
  mutate(
    teammate_gap_to_best_rank = rank[1] - rank,
    teammate_gap_to_2nd_best_rank = rank[2] - rank,
    teammate_gap_to_3rd_best_rank = rank[3] - rank,
    
    teammate_gap_to_2nd_best_rank = if_else(is.na(teammate_gap_to_2nd_best_rank), 410 - rank, teammate_gap_to_2nd_best_rank),
    teammate_gap_to_3rd_best_rank = if_else(is.na(teammate_gap_to_3rd_best_rank), 410 - rank, teammate_gap_to_3rd_best_rank),
    
    teammate_gap_to_next_best_rank = lead(rank, default = 0) - rank
    
  ) %>% 
  ungroup() %>% 
  
  arrange(season, week) %>% 
  group_by(merge_name, position) %>% 
  mutate(
    across(.cols = where(is.numeric) & !contains("season") & !contains("week") & !contains("roll"),
           .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
           .names = "{.col}_roll4"),
    
    # Deltas between current week and rolling4
    win_probability_diff = win_probability - win_probability_roll4,
    team_total_diff = team_total - team_total_roll4,
    game_total_diff = game_total - game_total_roll4,
    practice_injury_diff = practice_injury_count - practice_injury_count_roll4,
    report_injury_diff = report_injury_count - report_injury_count_roll4,
    rank_diff = rank - rank_roll4,
    sd_diff = sd - sd_roll4,
    team_position_rank_diff = team_position_rank - team_position_rank_roll4,
    teammate_best_rb_rank_diff = teammate_best_rb_rank - teammate_best_rb_rank_roll4,
    teammate_best_rec_rank_diff = teammate_best_rec_rank - teammate_best_rec_rank_roll4,
    teammate_2nd_best_rb_rank_diff = teammate_2nd_best_rb_rank - teammate_2nd_best_rb_rank_roll4,
    teammate_2nd_best_rec_rank_diff = teammate_2nd_best_rec_rank - teammate_2nd_best_rec_rank_roll4,
    teammate_3rd_best_rec_rank_diff = teammate_3rd_best_rec_rank - teammate_3rd_best_rec_rank_roll4,
    teammate_4th_best_rec_rank_diff = teammate_4th_best_rec_rank - teammate_4th_best_rec_rank_roll4,
    teammate_5th_best_rec_rank_diff = teammate_5th_best_rec_rank - teammate_5th_best_rec_rank_roll4,
    teammate_gap_to_best_rank_diff = teammate_gap_to_best_rank - teammate_gap_to_best_rank_roll4,
    teammate_gap_to_2nd_best_rank_diff = teammate_gap_to_2nd_best_rank - teammate_gap_to_2nd_best_rank_roll4,
    teammate_gap_to_3rd_best_rank_diff = teammate_gap_to_3rd_best_rank - teammate_gap_to_3rd_best_rank_roll4,
    teammate_gap_to_next_best_rank_diff = teammate_gap_to_next_best_rank - teammate_gap_to_next_best_rank_roll4,
    teammate_qb_rank_diff = teammate_qb_rank - teammate_qb_rank_roll4,
    
    # Rate stat rolling 4 week averages
    rush_ypc_roll4 = slide2_dbl(rush_yards_gained, rush_attempt, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_ypc_exp_roll4 = slide2_dbl(rush_yards_gained_exp, rush_attempt, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_ypc_diff_roll4 =  rush_ypc_roll4 - rush_ypc_exp_roll4,
    
    rush_attempt_share_roll4 = slide2_dbl(rush_attempt, rush_attempt_team, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_yard_share_roll4 = slide2_dbl(rush_yards_gained, rush_yards_gained_team, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_yard_share_exp_roll4 = slide2_dbl(rush_yards_gained_exp, rush_yards_gained_exp_team, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_yard_share_diff_roll4 = rush_yard_share_roll4 - rush_yard_share_exp_roll4,
    
    rush_first_down_rate_roll4 = slide2_dbl(rush_first_down, rush_attempt, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_first_down_rate_exp_roll4 = slide2_dbl(rush_first_down_exp, rush_attempt, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_first_down_rate_diff_roll4 =  rush_first_down_rate_roll4 - rush_first_down_rate_exp_roll4,
    
    rush_td_rate_roll4 = slide2_dbl(rush_touchdown, rush_attempt, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_td_rate_exp_roll4 = slide2_dbl(rush_touchdown_exp, rush_attempt, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_td_rate_diff_roll4 =  rush_td_rate_roll4 - rush_td_rate_exp_roll4,
    
    rush_ypta_roll4 = slide2_dbl(rush_yards_gained, rush_attempt_team, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_ypta_exp_roll4 = slide2_dbl(rush_yards_gained_exp, rush_attempt_team, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_ypta_diff_roll4 =  rush_ypta_roll4 - rush_ypta_exp_roll4,
    
    rush_fp_share_roll4 = slide2_dbl(rush_fantasy_points, rush_fantasy_points_team, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_fp_share_exp_roll4 = slide2_dbl(rush_fantasy_points_exp, rush_fantasy_points_exp_team, ~get_rate(.x,.y), .before = 4, .after = -1),
    rush_fp_share_diff_roll4 =  rush_fp_share_roll4 - rush_fp_share_exp_roll4,
    
    snap_share_roll4 = slide2_dbl(player_snaps, team_snaps, ~get_rate(.x,.y), .before = 4, .after = -1)
    
  )  %>%
  ungroup() %>% 
  filter(season >= 2013, position %in% c("RB", "QB")) %>% 
  select(
    season,
    week,
    game_id,
    position,
    merge_name,
    team,
    home_away,
    contains("win_probability"),
    contains("game_total"),
    contains("team_total"),
    contains("rank"),
    contains("sd"),
    contains("report"),
    contains("practice"),
    contains("roll"),
    rush_attempt,
    rush_yards_gained,
    rush_touchdown
  ) %>% 
  mutate(rush_attempt_factor = cut(rush_attempt, breaks = c(0:25,72),
                                   include.lowest = TRUE,
                                   right = FALSE),
         rush_attempt_factor = str_extract(rush_attempt_factor, "(?<=\\[)(.*)(?=\\,)"),
         rush_attempt_factor = if_else(rush_attempt_factor == "25", "25+", rush_attempt_factor),
         rush_yard_factor = cut(rush_yards_gained, breaks = c(-14,0:110,253),
                                include.lowest = TRUE,
                                right = FALSE),
         rush_yard_factor = str_extract(rush_yard_factor, "(?<=\\[)(.*)(?=\\,)"),
         
         rush_yard_factor = case_when(rush_yard_factor == "-14" ~ "<0",
                                      rush_yard_factor == "110" ~ "110+",
                                      TRUE ~ rush_yard_factor),
         
         rush_touchdown_factor = case_when(rush_touchdown == 0 ~ "0",
                                           rush_touchdown == 1 ~  "1",
                                           rush_touchdown >= 2 ~ "2+")
  )


# joined_weekly_df %>% 
#   filter(season == 2022, week == 7) %>% 
#   filter(merge_name == "Alvin Kamara") %>% 
#   view()

rush_attempts_fit <- readRDS("./models_xgboost/rush_attempts_fit.RDS")

library(tidymodels)
rush_att_preds_wide <-
  joined_weekly_df %>%
  filter(season == 2022, week == current_week) %>%
  bind_cols(predict(
    rush_attempts_fit,
    joined_weekly_df %>%
      filter(season == 2022, week == current_week),
    type = "prob") %>% 
      rename_with(.fn = ~ paste0("rush", .x)))

rush_att_preds_long <- 
  rush_att_preds_wide %>% 
  select(season, week, game_id, team, merge_name, position, rank, contains("pred")) %>% 
  pivot_longer(cols = contains("pred")) %>% 
  mutate(rush_attempt_pred = as.numeric(str_remove(name, "rush.pred_")),
         rush_attempt_pred = replace_na(rush_attempt_pred, 25)) %>% 
  arrange(season, week, merge_name, rush_attempt_pred) %>% 
  group_by(season, week, merge_name) %>% 
  mutate(market_name = "rushing_attempts",
         under_prob = cumsum(value),
         over_prob = 1 - under_prob,
         market_line =  rush_attempt_pred + 0.5,
         under_prob_us = round(odds.prob2us(under_prob)),
         over_prob_us = round(odds.prob2us(over_prob))) %>% 
  ungroup() %>% 
  rename(pred_category = rush_attempt_pred)

rush_train_summary <- 
  rush_att_preds_long %>% 
  group_by(season, week, position, merge_name, rank) %>% 
  summarise(expected_median_rush_attempts = matrixStats::weightedMedian(pred_category, w = value),
            expected_mean_rush_attempts = matrixStats::weightedMean(pred_category, w = value),
            probability_0_5_carries = sum(if_else(pred_category >=0 & pred_category <=5, value, 0)),
            probability_6_10_carries = sum(if_else(pred_category >=6 & pred_category <=10, value, 0)),
            probability_11_15_carries = sum(if_else(pred_category >=11 & pred_category <=15, value, 0)),
            probability_16_20_carries = sum(if_else(pred_category >=16 & pred_category <=20, value, 0)),
            probability_21_plus_carries = sum(if_else(pred_category >=21, value, 0))) %>% 
  ungroup()

# rush_train_preds_summary %>% 
#   group_by(position, season) %>% 
#   summarise(n(),
#             median(expected_median_rush_attempts),
#             mean(rush_attempt > expected_median_rush_attempts),
#             mean(rush_attempt < expected_median_rush_attempts)) %>% 
#   ungroup() %>% 
#   view()

rush_yards_prep <- 
  rush_att_preds_wide %>% 
  left_join(rush_train_summary, by = c("season", "week", "position", "merge_name"))
  # left_join(rush_train_summary %>% select(-rank), by = c("season", "week", "position", "merge_name"))


###################
rush_yards_fit <- readRDS("./models_xgboost/rush_yards_fit.RDS")

rush_yard_preds_wide <-
  rush_yards_prep %>%
  filter(season == 2022, week == current_week) %>%
  bind_cols(predict(
    rush_yards_fit,
    rush_yards_prep %>%
      filter(season == 2022, week == current_week),
    type = "prob") %>% 
    rename_with(.fn = ~ paste0("rush_yards", .x))
  )

rush_yards_preds_long <- 
  rush_yard_preds_wide %>% 
  select(season, week, game_id, team, merge_name, position, rank.x, contains("rush_yards.pred")) %>% 
  pivot_longer(cols = contains("rush_yards.pred_")) %>% 
  mutate(rush_yard_pred = str_remove(name, "rush_yards.pred_"),
         rush_yard_pred = case_when(rush_yard_pred == "<0" ~ "-1",
                                    rush_yard_pred == "110+" ~ "110",
                                    TRUE ~ rush_yard_pred),
         rush_yard_pred = as.numeric(rush_yard_pred)) %>% 
  arrange(season, week, merge_name, rush_yard_pred) %>% 
  group_by(season, week, merge_name) %>% 
  mutate(market_name = "rushing_yards",
         under_prob = cumsum(value),
         over_prob = 1 - under_prob,
         market_line =  rush_yard_pred + 0.5,
         under_prob_us = round(odds.prob2us(under_prob)),
         over_prob_us = round(odds.prob2us(over_prob))) %>% 
  ungroup() %>% 
  rename(pred_category = rush_yard_pred)

rush_yard_summary <- 
  rush_yards_preds_long %>% 
  group_by(season, week, position, merge_name, rank.x) %>% 
  summarise(expected_median_rush_yards = matrixStats::weightedMedian(pred_category, w = value),
            expected_mean_rush_yards = matrixStats::weightedMean(pred_category, w = value)) %>% 
  ungroup()

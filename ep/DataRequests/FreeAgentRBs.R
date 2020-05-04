library(tidyverse)
library(lubridate)

games <- read.csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv",
                     fileEncoding = "UTF-8-BOM")
database <- read.csv("https://raw.githubusercontent.com/tanho63/dynastyprocess-apps/dev/db/database.csv",
                     fileEncoding = "UTF-8-BOM")


setwd('C:/Users/syd23/OneDrive/Documents/dynastyprocess-apps/ep')

#Import 2009-2019 roster data
filelist <- grep('reg_roster', list.files(), value = TRUE)
rosters <- data.frame()
for (f in filelist){
  df <- read.csv(f)
  rosters <- bind_rows(rosters,df)
  rm(df,f)
}

rosters_cleaned <- rosters %>%
  mutate(Position = case_when(full_player_name == "Mike Sellers" ~ "RB",
                              full_player_name == "James Casey" ~ "TE",
                              full_player_name == "Dexter McCluster" ~ "RB",
                              full_player_name == "Joe Webb" ~ "QB",
                              full_player_name == "Dorin Dickerson" ~ "TE",
                              full_player_name == "Niles Paul" ~ "TE",
                              full_player_name == "Terrelle Pryor" ~ "WR",
                              full_player_name == "De'Anthony Thomas" ~ "WR",
                              full_player_name == "Logan Thomas" ~ "TE",
                              full_player_name == "Darren Waller" ~ "TE",
                              full_player_name == "Daniel Brown" ~ "TE",
                              full_player_name == "Neal Sterling" ~ "TE",
                              full_player_name == "Ty Montgomery" ~ "RB",
                              full_player_name == "Byron Marshall" ~ "RB",
                              full_player_name == "Danny Woodhead" ~ "RB",
                              full_player_name == "Ryan Hewitt" ~ "RB",
                              full_player_name == "Vince Mayle" ~ "WR",
                              full_player_name == "Ernest Wilford" ~ "WR",
                              gsis_id == "00-0023474" ~ "WR",                       #Mark Bradley 2009
                              gsis_id == "00-0029055" ~ "WR",                       #Kevin Elliott 2012
                              position == "FB" ~ "RB",
                              TRUE ~ as.character(position))) %>%
  dplyr::select(full_player_name, Position, gsis_id) %>%
  distinct()

#Import 2009-2019 pbp data
filelist <- grep('reg_pbp', list.files(), value = TRUE)
pbp <- data.frame()
for (f in filelist){
  df <- read.csv(f)
  pbp <- bind_rows(pbp,df)
  rm(df,f)
}

#Pull in passer, rusher, and receiver positions
pbp <- pbp %>%
  left_join(dplyr::select(rosters_cleaned,
                          rusher_gsis_id = gsis_id,
                          rusher_gsis_name = full_player_name,
                          rusher_gsis_pos = Position),
            by = c("rusher_player_id" = "rusher_gsis_id")) %>%
  left_join(dplyr::select(rosters_cleaned,
                          receiver_gsis_id = gsis_id,
                          receiver_gsis_name = full_player_name,
                          receiver_gsis_pos = Position),
            by = c("receiver_player_id" = "receiver_gsis_id")) %>%
  left_join(dplyr::select(rosters_cleaned,
                          passer_gsis_id = gsis_id,
                          passer_gsis_name = full_player_name,
                          passer_gsis_pos = Position),
            by = c("passer_player_id" = "passer_gsis_id")) %>%
  left_join(games, by = "game_id")

#Filter to RBs that changed teams
filteredRB <- pbp %>%
  filter((rusher_gsis_pos == "RB" | receiver_gsis_pos == "RB") & (play_type %in% c("pass","run"))) %>%
  mutate(rb_gsis_id = coalesce(rusher_player_id, receiver_player_id),
         rb_gsis_name = coalesce(rusher_gsis_name, receiver_gsis_name),
         rush = ifelse(play_type == "run",1,0),
         rushyds = ifelse(play_type == "run",yards_gained,0),
         recyds = ifelse(play_type == "pass",yards_gained,0),
         NextSeason = season + 1) %>%
  group_by(rb_gsis_id, rb_gsis_name, NextSeason, season) %>%
  mutate(Team = paste(unique(posteam), collapse = "/")) %>%
  ungroup() %>%
  group_by(rb_gsis_id, rb_gsis_name, season, NextSeason, Team) %>%
  summarise(Games = n_distinct(game_id),
            Rushes = sum(rush, na.rm = TRUE),
            RushYards = sum(rushyds, na.rm = TRUE),
            RushTDs = sum(rush_touchdown, na.rm = TRUE),
            Targets = sum(pass_attempt, na.rm = TRUE),
            Receptions = sum(complete_pass, na.rm = TRUE),
            RecYards = sum(recyds, na.rm = TRUE),
            RecTDs = sum(pass_touchdown, na.rm = TRUE),
            FirstDowns = sum(first_down_rush + first_down_pass, na.rm = TRUE),
            Fumbles = sum(fumble_lost, na.rm = TRUE),
            FantasyPointsPPR = sum(0.1*yards_gained + 6*pass_touchdown + 6*rush_touchdown +
                                     1*complete_pass - 2*fumble_lost),
            PPRperGame = FantasyPointsPPR / Games
            ) %>%
  mutate_if(is.numeric,round,digits=2)

multiple_teams <- filteredRB %>%
  inner_join(filteredRB,
             by = c("rb_gsis_id","NextSeason"="season"),
             suffix = c("","_y1"),
             keep = TRUE) %>%
  filter(Team != Team_y1) %>%
  left_join(select(database, gsis_id, birthdate),
            by = c("rb_gsis_id" = "gsis_id")) %>%
  filter(is.na(birthdate))
  
  
  
filteredRB %>%
  group_by(play_type) %>%
  summarise(n = n())
  
pbp %>%
  group_by(weekday) %>%
  mutate(teams = paste(posteam, collapse = "_"))
  
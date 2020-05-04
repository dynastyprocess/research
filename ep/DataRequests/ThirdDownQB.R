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
            by = c("passer_player_id" = "passer_gsis_id"))

#Aggregate Date
ThirdDown <- pbp %>%
  filter(down == 3 & play_type == "pass" & passer_gsis_pos == "QB") %>%
  group_by(year(game_date),passer_gsis_name, posteam) %>%
  summarise(
    Attempts = sum(pass_attempt, na.rm = TRUE),
    Completions = sum(complete_pass, na.rm = TRUE),
    CompletionPercentage = mean(complete_pass, na.rm = TRUE),
    FirstDowns = sum(first_down_pass, na.rm = TRUE),
    ConversionPercentage = mean(first_down_pass, na.rm = TRUE),
    Sacks = sum(sack, na.rm = TRUE),
    SackRate = mean(sack, na.rm = TRUE),
    Interceptions = sum(interception, na.rm = TRUE),
    IntRate = mean(interception, na.rm = TRUE),
    TotalPassYards = sum(yards_gained, na.rm = TRUE),
    YPA = TotalPassYards/Attempts,
    aDOT = mean(air_yards, na.rm = TRUE),
    AverageYardsToGo = mean(ydstogo, na.rm = TRUE)) %>%
  mutate_if(is.numeric,round,digits=3) %>%
  dplyr::select(Year = `year(game_date)`,
                QBName = passer_gsis_name,
                Team = posteam,
                Attempts,
                Completions,
                CompletionPercentage,
                FirstDowns,
                ConversionPercentage,
                Sacks,
                SackRate,
                Interceptions,
                IntRate,
                TotalPassYards,
                YPA,
                aDOT,
                AverageYardsToGo) %>%
  arrange(Year, -Attempts)

write.csv(ThirdDown, file = "ThirdDownPassing.csv")
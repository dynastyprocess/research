library(tidyverse)
library(nflscrapR)
library(ggplot2)
library(pscl)
library(lubridate)
library(stargazer)
library(MASS)
library(mgcv)
library(zoo)
# library(car)
# library(DHARMa)
# library(fitdistrplus)
library(ggpubr)
#library(RSQLite)

#download.file("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2019.csv","reg_roster_2019.csv")
games <- read.csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv",
                  fileEncoding = "UTF-8-BOM")

#setwd("~/GitHub/DynastyProcess-Apps/ep")
setwd('C:/Users/syd23/OneDrive/Documents/dynastyprocess-apps/ep')

set.seed(100)  # setting seed to reproduce results of random sampling

#Import 2009-2018 roster data
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

#Import 2009-2018 pbp data
filelist <- grep('reg_pbp', list.files(), value = TRUE) #%>% .[1:10]
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
            by = c("passer_player_id" = "passer_gsis_id"))%>%
  left_join(games, by = "game_id")

#Split rush and rec dataframes
rushdf <- pbp %>% 
  filter(play_type == "run" & two_point_attempt == 0 & !grepl("kneel",desc)) %>%
  mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         rushFP = 6*rush_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost,
         rushFP1D = 6*rush_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + 0.5*first_down_rush,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         run_gap = ifelse(is.na(run_gap), "center", as.character(run_gap)),
         run_location = ifelse(is.na(run_location), "unknown", as.character(run_location)),
         run_gap_dir = paste0(run_location,run_gap),
         run_gap_dir = case_when(run_gap_dir %in% c("leftcenter","rightcenter","middleend") |
                                   run_location == "unknown" ~ "unknown",
                                 TRUE ~ run_gap_dir)
  )

recdf <- pbp %>% 
  filter(play_type == "pass" & sack == 0 & two_point_attempt == 0) %>%
  mutate(TwoPtConv = if_else(two_point_conv_result == 'success', 1, 0, missing = 0),
         recFP = 6*pass_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         recFP1D = 6*pass_touchdown + 2*TwoPtConv + 0.1*yards_gained - 2*fumble_lost + complete_pass+ 0.5*first_down_pass,
         passFP =  4*pass_touchdown + 2*TwoPtConv + 0.04*yards_gained - 2*fumble_lost - 2*interception,
         logyardline = log(yardline_100),
         yardlinesq = yardline_100*yardline_100,
         abs_air_yards = case_when(abs(air_yards)>yardline_100 ~ yardline_100,
                                   TRUE ~ abs(air_yards)),
         pass_location = ifelse(is.na(pass_location), "unknown", as.character(pass_location)),
         targetline = yardline_100 - air_yards)

#Reception Model
recModGam <- gam(complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20)  + receiver_gsis_pos + ti(air_yards,yardline_100),
                 data=recdf, method = "REML", family=binomial(link="logit"), select = TRUE)

recdf$eRecGam <- predict(recModGam, recdf, type="response")

#Receiving Yards Model
recYDModGam <- gam(yards_gained ~ s(air_yards) + s(eRecGam) + pass_location,
                   data=recdf, method = "REML", select = TRUE)

recdf$eRecYDGam <- predict(recYDModGam, recdf)
recdf$eRecYDGam = ifelse(recdf$eRecYDGam > recdf$yardline_100,recdf$yardline_100,recdf$eRecYDGam)

#Receiving TD Model
recTDModGam <- gam(pass_touchdown ~ s(yardline_100, k=15) + s(eRecGam, k=15) + s(eRecYDGam, k=15),
                   data=recdf, method = "REML", family=binomial(link="logit"), select = TRUE)

recdf$eRecTDGam <- predict(recTDModGam, recdf, type="response")

#Receiving FP Model
recFPModGam <- gam(recFP ~ s(eRecTDGam, k = 30) + s(eRecYDGam, k = 20) + s(eRecGam, k = 20) +
                      ti(eRecGam, eRecYDGam, k = 9) + ti(eRecGam, eRecTDGam, k = 6),
                    data=recdf, method = "REML", select = TRUE)
recdf$eRecFPGam <- predict(recFPModGam, recdf)

#Passing FP Model
passFPModGam <- gam(passFP ~ s(eRecTDGam, k = 35) + s(eRecYDGam, k = 20) + s(eRecGam, k = 25) +
                      ti(eRecGam, eRecYDGam, k = 10) + ti(eRecGam, eRecTDGam, k = 8),
                    data=recdf, method = "REML", select = TRUE)

recdf$ePassFPGam <- predict(passFPModGam, recdf)

#Rushing Yards Model
rushYDModGam <- gam(yards_gained ~ s(yardline_100) + rusher_gsis_pos + shotgun + run_gap_dir,
                    data=rushdf, method = "REML", select = TRUE)
rushdf$eRushYDGam <- predict(rushYDModGam, rushdf)
rushdf$eRushYDGam = ifelse(rushdf$eRushYDGam > rushdf$yardline_100,rushdf$yardline_100,rushdf$eRushYDGam)

#Rushing TD Model
rushTDModGam <- gam(rush_touchdown ~ s(yardline_100) + s(eRushYDGam),
                    data=rushdf, method = "REML", family=binomial(link="logit"), select = TRUE)

rushdf$eRushTDGam <- predict(rushTDModGam, rushdf, type = "response")

#Rushing FP Model
rushFPModGam <- gam(rushFP ~ s(eRushTDGam, k = 20) + s(eRushYDGam, k = 20) + ti(eRushTDGam,eRushYDGam, k = 8),
                     data=rushdf, method = "REML", select = TRUE)

rushdf$eRushFPGam <- predict(rushFPModGam, rushdf)

#Save data overnight
write.csv(recdf, "recdf.csv")
write.csv(rushdf, "rushdf.csv")

recModGam[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recYDModGam[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recTDModGam[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL

recFPModGam[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
passFPModGam[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL

rushYDModGam[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rushTDModGam[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rushFPModGam[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL

save(recModGam, recYDModGam, recTDModGam, recFPModGam, passFPModGam,
     rushYDModGam, rushTDModGam, rushFPModGam, file = "newmodels.rda")
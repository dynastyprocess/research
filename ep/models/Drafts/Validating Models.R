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
library(ggrepel)

#download.file("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/roster_data/regular_season/reg_roster_2019.csv","reg_roster_2019.csv")
games <- read.csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv",
                  fileEncoding = "UTF-8-BOM")

setwd("~/OneDrive/Documents/dynastyprocess-apps/ep")
recdf <- read.csv("recdf.csv")
rushdf <- read.csv("rushdf.csv")

#combine Game level rushing, receiving, and passing data
rushGame <- rushdf %>%
  filter(!is.na(rusher_gsis_name)) %>%
  group_by(game_id, rusher_player_id, rusher_gsis_name, rusher_gsis_pos) %>%
  summarise(
    rushFPgame = sum(rushFP, na.rm = TRUE),
    erushFPgame = sum(eRushFPGam, na.rm = TRUE))%>%
  ungroup()

recGame <- recdf %>%
  filter(!is.na(receiver_gsis_name)) %>%
  group_by(game_id, receiver_player_id, receiver_gsis_name, receiver_gsis_pos) %>%
  summarise(
    recFPgame = sum(recFP, na.rm = TRUE),
    erecFPgame = sum(eRecFPGam, na.rm = TRUE))%>%
  ungroup()

passGame <- recdf %>%
  filter(!is.na(passer_gsis_name)) %>%
  group_by( game_id, passer_player_id, passer_gsis_name, passer_gsis_pos) %>%
  summarise(
    passFPgame = sum(passFP, na.rm = TRUE),
    epassFPgame = sum(ePassFPGam, na.rm = TRUE))%>%
  ungroup()

combinedf <-
  full_join(rushGame, recGame, by=c("rusher_player_id" = "receiver_player_id", "game_id")) %>%
  mutate(combo_id = ifelse(is.na(rusher_player_id), receiver_player_id, rusher_player_id),
         combo_name = ifelse(is.na(rusher_gsis_name), as.character(receiver_gsis_name), as.character(rusher_gsis_name)),
         combo_pos = ifelse(is.na(rusher_gsis_pos), as.character(receiver_gsis_pos), as.character(rusher_gsis_pos))) %>%
  full_join(passGame, by=c("combo_id" = "passer_player_id", "game_id")) %>%
  mutate(player_id = ifelse(is.na(combo_id), passer_player_id, combo_id),
         gsis_name = ifelse(is.na(combo_name), as.character(passer_gsis_name), as.character(combo_name)),
         gsis_pos = ifelse(is.na(combo_pos), as.character(passer_gsis_pos), as.character(combo_pos))) %>%
  left_join(dplyr::select(games, season, week, game_id), by="game_id") %>%
  rowwise() %>%
  mutate(totalFPgame = sum(passFPgame, rushFPgame, recFPgame, na.rm = TRUE),
            etotalFPgame = sum(epassFPgame, erushFPgame, erecFPgame, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(season, week, game_id, player_id, gsis_name, gsis_pos, totalFPgame, etotalFPgame,
                passFPgame, epassFPgame, rushFPgame, erushFPgame, recFPgame, erecFPgame)

combinedf[is.na(combinedf)] <- 0

gameThreshold <- combinedf %>%
  filter(season < 2019) %>%
  group_by(player_id) %>%
  summarise(
    passGames = sum(passFPgame > 0),
    rushGames = sum(rushFPgame > 0),
    recGames = sum(recFPgame > 0),
    totalGames = sum(totalFPgame > 0)) %>%
  ungroup()

#top60------------------
# recYearFP <- recGameFP %>%
#   group_by(season, receiver_player_id, receiver_gsis_name, receiver_gsis_pos) %>%
#   summarise(
#     games = n(),
#     PPG = mean(recFPgame, na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(games >= 4 & receiver_gsis_pos != 'QB') %>%
#   group_by(season, receiver_gsis_pos) %>%
#   mutate(PPGRank = rank(-PPG, ties.method = "first")) %>%
#   ungroup() %>%
#   filter(PPGRank <= 60) %>%
#   dplyr::select(season, receiver_player_id)
#Cumulative and ROS loops ------------------
dffit <- data.frame()
for (i in seq(8,8,0)) { #Last i Weeks
  for (j in seq(8,8,0)) { #Next j Weeks
    #for (k in c(9:16)) { #In game k
      gameAvg <- combinedf %>%
        #filter(season < 2019) %>%
        #inner_join(gameThreshold, by = c("player_id")) %>%
        group_by(player_id, gsis_name, gsis_pos) %>%
        arrange(game_id) %>%
        mutate(
          RowNum = row_number(),
          #Receiving averages
          #recFPgame_lastx = rollapplyr(recFPgame, list(-(i:1)), mean, fill=NA),
          recFPgame_lastx = rollapplyr(recFPgame, list(-((i-1):0)), mean, fill=NA),
          erecFPgame_lastx = rollapplyr(erecFPgame, list(-(i:1)), mean, fill=NA),
          diffrecgame_lastx = recFPgame_lastx - erecFPgame_lastx,
          recFPgame_nexty = rollapplyr(recFPgame, list((1:j)), mean, fill=NA),
          recFPgame_last1 = lag(recFPgame, default = 0),
          erecFPgame_last1 = lag(erecFPgame, default = 0),
          recFPgame_next1 = lead(recFPgame, default = 0),
          recFPgame_ytd = cumsum(recFPgame_last1)/lag(seq_along(recFPgame_last1)),
          erecFPgame_ytd= cumsum(erecFPgame_last1)/lag(seq_along(erecFPgame_last1)),
          recFPgame_roc = rev(cumsum(rev(recFPgame_next1)))/lead(rev(seq_along(rev(recFPgame_next1)))),
          
          #Rusing averages
          #rushFPgame_lastx = rollapplyr(rushFPgame, list(-(i:1)), mean, fill=NA),
          rushFPgame_lastx = rollapplyr(rushFPgame, list(-((i-1):0)), mean, fill=NA),
          erushFPgame_lastx = rollapplyr(erushFPgame, list(-(i:1)), mean, fill=NA),
          diffrushgame_lastx = rushFPgame_lastx - erushFPgame_lastx,          
          rushFPgame_nexty = rollapplyr(rushFPgame, list((1:j)), mean, fill=NA),
          rushFPgame_last1 = lag(rushFPgame, default = 0),
          erushFPgame_last1 = lag(erushFPgame, default = 0),
          rushFPgame_next1 = lead(rushFPgame, default = 0),
          rushFPgame_ytd = cumsum(rushFPgame_last1)/lag(seq_along(rushFPgame_last1)),
          erushFPgame_ytd= cumsum(erushFPgame_last1)/lag(seq_along(erushFPgame_last1)),
          rushFPgame_roc = rev(cumsum(rev(rushFPgame_next1)))/lead(rev(seq_along(rev(rushFPgame_next1)))),
          
          #Passing averages
          #passFPgame_lastx = rollapplyr(passFPgame, list(-(i:1)), mean, fill=NA),
          passFPgame_lastx = rollapplyr(passFPgame, list(-((i-1):0)), mean, fill=NA),
          epassFPgame_lastx = rollapplyr(epassFPgame, list(-(i:1)), mean, fill=NA),
          diffpassgame_lastx = passFPgame_lastx - epassFPgame_lastx,
          passFPgame_nexty = rollapplyr(passFPgame, list((1:j)), mean, fill=NA),
          passFPgame_last1 = lag(passFPgame, default = 0),
          epassFPgame_last1 = lag(epassFPgame, default = 0),
          passFPgame_next1 = lead(passFPgame, default = 0),
          passFPgame_ytd = cumsum(passFPgame_last1)/lag(seq_along(passFPgame_last1)),
          epassFPgame_ytd= cumsum(epassFPgame_last1)/lag(seq_along(epassFPgame_last1)),
          passFPgame_roc = rev(cumsum(rev(passFPgame_next1)))/lead(rev(seq_along(rev(passFPgame_next1)))),
          
          #Total averages
          #totalFPgame_lastx = rollapplyr(totalFPgame, list(-(i:1)), mean, fill=NA),
          totalFPgame_lastx = rollapplyr(totalFPgame, list(-((i-1):0)), mean, fill=NA),
          etotalFPgame_lastx = rollapplyr(etotalFPgame, list(-(i:1)), mean, fill=NA),
          difftotalgame_lastx = totalFPgame_lastx - etotalFPgame_lastx,
          totalFPgame_nexty = rollapplyr(totalFPgame, list((1:j)), mean, fill=NA),
          totalFPgame_last1 = lag(totalFPgame, default = 0),
          etotalFPgame_last1 = lag(etotalFPgame, default = 0),
          totalFPgame_next1 = lead(totalFPgame, default = 0),
          totalFPgame_ytd = cumsum(totalFPgame_last1)/lag(seq_along(totalFPgame_last1)),
          etotalFPgame_ytd= cumsum(etotalFPgame_last1)/lag(seq_along(etotalFPgame_last1)),
          totalFPgame_roc = rev(cumsum(rev(totalFPgame_next1)))/lead(rev(seq_along(rev(totalFPgame_next1))))
          )
      # %>%
      #   ungroup() %>% 
      #   group_by(season, receiver_player_id, receiver_gsis_name, receiver_gsis_pos) %>%
      #   mutate(
      #     rosFPavg = rev(cumsum(rev(NextFP)))/lead(rev(seq_along(rev(NextFP)))),
      #   ) %>%
      #   ungroup()
      # %>% filter(RowNum == k)
      recGameAvg <- gameThreshold %>%
        filter(recGames >= 32) %>%
        inner_join(gameAvg, by="player_id")
      
      rushGameAvg <- gameThreshold %>%
        filter(rushGames >= 32) %>%
        inner_join(gameAvg, by="player_id")
      
      passGameAvg <- gameThreshold %>%
        filter(passGames >= 32) %>%
        inner_join(gameAvg, by="player_id")
      
      totalGameAvg <- gameThreshold %>%
        filter(totalGames >= 32) %>%
        inner_join(gameAvg, by="player_id")
      
      recMod <-  gam(recFPgame_nexty ~ s(recFPgame_lastx) + s(erecFPgame_lastx)
                      + s(recFPgame_ytd) + s(erecFPgame_ytd)
                      + s(RowNum) + ti(RowNum, recFPgame_ytd) + ti(RowNum,erecFPgame_ytd),
                      data = recGameAvg, method = "REML", select = TRUE)

      rushMod <- gam(rushFPgame_nexty ~ s(rushFPgame_lastx) + s(erushFPgame_lastx)
                      + s(rushFPgame_ytd) + s(erushFPgame_ytd)
                      + s(RowNum) + ti(RowNum,rushFPgame_ytd) + ti(RowNum,erushFPgame_ytd),
                      data=rushGameAvg, method = "REML", select = TRUE)

      passMod <- gam(passFPgame_nexty ~ s(passFPgame_lastx) + s(epassFPgame_lastx)
                      + s(passFPgame_ytd) + s(epassFPgame_ytd)
                      + s(RowNum) + ti(RowNum,passFPgame_ytd) + ti(RowNum,epassFPgame_ytd),
                      data=passGameAvg, method = "REML", select = TRUE)

      totalMod <- gam(totalFPgame_nexty ~ s(totalFPgame_lastx) + s(etotalFPgame_lastx)
                       + s(totalFPgame_ytd) + s(etotalFPgame_ytd)
                       + s(RowNum) + ti(RowNum,totalFPgame_ytd) + ti(RowNum,etotalFPgame_ytd),
                       data=totalGameAvg, method = "REML", select = TRUE)
      
      recMod2 <-  gam(recFPgame_roc ~ s(recFPgame_lastx) + s(erecFPgame_lastx) 
                      + s(recFPgame_ytd) + s(erecFPgame_ytd)
                      + s(RowNum) + ti(RowNum, recFPgame_ytd) + ti(RowNum, erecFPgame_ytd),
                      data = recGameAvg, method = "REML", select = TRUE)
      
      rushMod2 <- gam(rushFPgame_roc ~ s(rushFPgame_lastx) + s(erushFPgame_lastx) 
                      + s(rushFPgame_ytd) + s(erushFPgame_ytd)
                      + s(RowNum) + ti(RowNum, rushFPgame_ytd) + ti(RowNum, erushFPgame_ytd),
                      data=rushGameAvg, method = "REML", select = TRUE) 
      
      passMod2 <- gam(passFPgame_roc ~ s(passFPgame_lastx) + s(epassFPgame_lastx) 
                      + s(passFPgame_ytd) + s(epassFPgame_ytd)
                      + s(RowNum) + ti(RowNum, passFPgame_ytd) + ti(RowNum, epassFPgame_ytd),
                      data=passGameAvg, method = "REML", select = TRUE)
      
      totalMod2 <- gam(totalFPgame_roc ~ s(totalFPgame_lastx) + s(etotalFPgame_lastx)
                       + s(totalFPgame_ytd) + s(etotalFPgame_ytd)
                       + s(RowNum) + ti(RowNum, totalFPgame_ytd) + ti(RowNum, etotalFPgame_ytd),
                       data=totalGameAvg, method = "REML", select = TRUE)      
      
      dffit <- rbind(dffit,
                     data.frame(i,
                                summary(recMod)$r.sq,
                                summary(rushMod)$r.sq,
                                summary(passMod)$r.sq,
                                summary(totalMod)$r.sq,
                                summary(recMod2)$r.sq,
                                summary(rushMod2)$r.sq,
                                summary(passMod2)$r.sq,
                                summary(totalMod2)$r.sq,     
                                #summary(recMod2)$adj.r.squared,
                                #summary(recMod3)$adj.r.squared,
                                #summary(recMod4)$adj.r.squared,    
                                length(fitted(recMod)),
                                length(fitted(rushMod)),
                                length(fitted(passMod)),
                                length(fitted(totalMod)),
                                #length(fitted(recMod4)),
                                #InWeek = k,
                                LastX = i,
                                NextX = j
                                ))
    }
  }
#}

  dffit <- dffit%>%
    rename(recMod = summary.recMod..r.sq,
           rushMod = summary.rushMod..r.sq,
           passMod = summary.passMod..r.sq,
           totalMod = summary.totalMod..r.sq,
           recMod2 = summary.recMod2..r.sq,
           rushMod2 = summary.rushMod2..r.sq,
           passMod2 = summary.passMod2..r.sq,
           totalMod2 = summary.totalMod2..r.sq
           )

summary(passMod)
summary(passMod2)
plot(passMod)
vis.gam(passMod, view = c("RowNum","passFPgame_ytd"), plot.type = "contour", too.far=.1)

temp <- passGameAvg %>%
  filter(passFPgame_ytd >=19 & RowNum >=125)

temp <- passGameAvg %>%
  filter(passFPgame_ytd <=10 & RowNum >=50 & RowNum <=100 & passFPgame_ytd >=5 )

summary(recMod)
summary(recMod2)
plot(recMod)
vis.gam(recMod2, view = c("RowNum","recFPgame_ytd"), plot.type = "contour", too.far=.1)
temp <- recGameAvg %>%
  filter(recFPgame_ytd <=5 & RowNum >=100)

summary(rushMod)
summary(rushMod2)
vis.gam(rushMod2, view = c("RowNum","rushFPgame_ytd"), plot.type = "contour", too.far=.1)
temp <- rushGameAvg %>%
  filter(rushFPgame_ytd <=4 & RowNum >=120)

summary(totalMod)
summary(totalMod2)

# Plots -------------------------------------------------------------------

ggplot(dffit) + 
  geom_line(aes(x = LastX, y = recMod, color = 'a'), size = 2) + 
  geom_line(aes(x = LastX, y = rushMod, color = 'b'), size = 2) +
  geom_line(aes(x = LastX, y = passMod, color = 'c'), size = 2) +
  geom_line(aes(x = LastX, y = totalMod, color = 'd'), size = 2) +
  theme_minimal() +
  scale_x_continuous("LastX", breaks = c(1:16)) +
  scale_colour_manual(name = 'Model', 
                      values =c('a'='blue','b'='red','c'='black','d'='purple'),
                      labels = c('recMod','rushMod','passMod','totalMod')) +
  labs(title = "Predicting Next 16 games with Last X Games",
       y = "R-squared")

ggplot(dffit) + 
  geom_line(aes(x = LastX, y = recMod, color = 'a'), size = 2) + 
  geom_line(aes(x = LastX, y = rushMod, color = 'b'), size = 2) +
  geom_line(aes(x = LastX, y = passMod, color = 'c'), size = 2) +
  geom_line(aes(x = LastX, y = totalMod, color = 'd'), size = 2) +
  geom_line(aes(x = LastX, y = recMod2, color = 'a'), size = 2, linetype = "dashed") + 
  geom_line(aes(x = LastX, y = rushMod2, color = 'b'), size = 2, linetype = "dashed") +
  geom_line(aes(x = LastX, y = passMod2, color = 'c'), size = 2, linetype = "dashed") +
  geom_line(aes(x = LastX, y = totalMod2, color = 'd'), size = 2, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous("LastX", breaks = c(1:16)) +
  scale_colour_manual(name = 'Model', 
                      values =c('a'='blue','b'='red','c'='black','d'='purple'),
                      labels = c('recMod','rushMod','passMod','totalMod')) +
  labs(title = "Predicting Next 8 games with Last X Games",
       y = "R-squared")

ggplot(dffit) + 
  geom_line(aes(x = NextX, y = recMod, color = 'a'), size = 2) + 
  geom_line(aes(x = NextX, y = rushMod, color = 'b'), size = 2) +
  geom_line(aes(x = NextX, y = passMod, color = 'c'), size = 2) +
  geom_line(aes(x = NextX, y = totalMod, color = 'd'), size = 2) +
  theme_minimal() +
  scale_x_continuous("NextX", breaks = c(1:16)) +
  scale_colour_manual(name = 'Model', 
                      values =c('a'='blue','b'='red','c'='black','d'='purple'),
                      labels = c('recMod','rushMod','passMod','totalMod')) +
  labs(title = "Predicting Next X games with Last 16 Games",
       y = "R-squared")


ggplot(filter(recGameAvg, receiver_gsis_name == "Odell Beckham")) + 
  geom_line(aes(x = RowNum, y = ytdFPavg), color = 'blue') + 
  geom_line(aes(x = RowNum, y = ytdEPavg), color = 'red') +
  geom_line(aes(x = RowNum, y = rosFPavg), color = 'black') +
  scale_x_continuous("Game",breaks = seq(0,160,16)) +
  ylab("FP/EP per game") +
  theme_minimal()

ggplot(filter(recGameAvg, receiver_gsis_name == "Odell Beckham")) + 
  geom_line(aes(x = RowNum, y = Last8FPmean), color = 'blue') + 
  geom_line(aes(x = RowNum, y = Last8EPmean), color = 'red') +
  geom_line(aes(x = RowNum, y = Next8FPmean), color = 'green') +
  geom_line(aes(x = RowNum, y = Next16FPmean), color = 'pink') +
  geom_line(aes(x = RowNum, y = rosFPavg), color = 'black') +
  geom_line(aes(x = RowNum, y = rocFPavg), color = 'purple') +  
  scale_x_continuous("Game",breaks = seq(0,160,16)) +
  ylab("FP/EP per game") +
  theme_minimal()

ggplot(filter(recGameAvg, receiver_gsis_name == "Larry Fitzgerald")) + 
  geom_line(aes(x = RowNum, y = LastXFPmean), color = 'blue') + 
  geom_line(aes(x = RowNum, y = LastXEPmean), color = 'red') +
  geom_line(aes(x = RowNum, y = NextXFPmean), color = 'black') +
  scale_x_continuous("Game",breaks = seq(0,160,16)) +
  ylab("FP/EP per game") +
  theme_minimal()

# Predictions -------------------------------------------------------------
gameAvg$recFPgame_pred8 <- predict(recMod, gameAvg)
gameAvg$rushFPgame_pred8 <- predict(rushMod, gameAvg)
gameAvg$passFPgame_pred8 <- predict(passMod, gameAvg)
gameAvg$totalFPgame_pred8 <- predict(totalMod, gameAvg)

gameAvg$recFPgame_predroc <- predict(recMod2, gameAvg)
gameAvg$rushFPgame_predroc <- predict(rushMod2, gameAvg)
gameAvg$passFPgame_predroc <- predict(passMod2, gameAvg)
gameAvg$totalFPgame_predroc <- predict(totalMod2, gameAvg)

LatestPrediction <- gameAvg %>%
  filter(season == 2018 & !is.na(totalFPgame_pred8)) %>%
  group_by(player_id) %>%
  mutate(maxgame_id = max(game_id)) %>%
  ungroup() %>%
  dplyr::select(player_id, maxgame_id) %>%
  distinct()

ExplorePred <- gameAvg %>%
  inner_join(LatestPrediction, by = c("player_id","game_id" = "maxgame_id")) %>%
  #mutate(diff = recFPgame_pred8 - recFPgame_predroc) %>% 
  dplyr::select(gsis_name, gsis_pos, week, ends_with("pred8"), ends_with("predroc"), ends_with("last8"), ends_with("ytd"))  %>%
  mutate_if(is.numeric, round, 1)

ExplorePred <- gameAvg %>%
  inner_join(LatestPrediction, by = c("player_id","game_id" = "maxgame_id")) %>%
  mutate(diff = recFPgame_pred8 - recFPgame_predroc) %>% 
  dplyr::select(gsis_name, gsis_pos, week, recFPgame_nexty, recFPgame_pred8, recFPgame_predroc, diff
                , recFPgame_lastx, erecFPgame_lastx, recFPgame_ytd, erecFPgame_ytd, RowNum, recFPgame_roc,
                ) %>%
  mutate_if(is.numeric, round, 1) %>%
  filter(recFPgame_pred8 >= 5 | recFPgame_nexty >= 5) %>%
  #dplyr::select(gsis_name, gsis_pos, week, ends_with("pred8"), ends_with("predroc"))  %>%
  #filter(gsis_pos == "WR")
  filter(gsis_pos == "RB")

mod <- lm(recFPgame_nexty ~ recFPgame_pred8, ExplorePred)
summary(mod)

ggplot(ExplorePred, aes(x = recFPgame_nexty, y = recFPgame_pred8, label = gsis_name)) + 
  geom_text_repel() +
  geom_point() + 
  geom_abline() +
  xlim(5,20) +
  ylim(5,20) +
  labs(title = "Comparing Predicted Next 8 vs Actual Next 8 end of 2018 season")
  
ExplorePred <- gameAvg %>%
  inner_join(LatestPrediction, by = c("player_id","game_id" = "maxgame_id")) %>%
  mutate(diff = totalFPgame_pred8 - totalFPgame_predroc) %>% 
  dplyr::select(gsis_name, gsis_pos, week, totalFPgame_pred8, totalFPgame_predroc, diff
                ,totalFPgame_lastx, etotalFPgame_lastx, totalFPgame_ytd, etotalFPgame_ytd, RowNum
                ) %>%
  mutate_if(is.numeric, round, 1) %>%
  #dplyr::select(gsis_name, gsis_pos, week, ends_with("pred8"), ends_with("predroc"))  %>%
  #filter(gsis_pos == "RB")
  filter(gsis_pos == 'RB')

temp <- gameAvg %>%
  filter(gsis_name == "JuJu Smith-Schuster") %>%
  dplyr::select(gsis_name, RowNum, week, recFPgame, recFPgame_lastx, recFPgame_nexty, recFPgame_pred8, recFPgame_predroc)

temp <- gameAvg %>%
  ungroup() %>%
  filter(gsis_pos == "QB") %>%
  mutate(diff = totalFPgame_pred8 - totalFPgame_predroc) %>%
  filter(diff < -1) %>%
  dplyr::select(gsis_name, gsis_pos, week, totalFPgame_pred8, totalFPgame_predroc, diff
                ,totalFPgame_lastx, etotalFPgame_lastx, totalFPgame_ytd, etotalFPgame_ytd, RowNum
  )

hist(as.numeric(temp$RowNum))
  
ggplot(temp) + 
    geom_point(aes(x = totalFPgame_pred8, y = totalFPgame_predroc, color = RowNum))

temp <- gameAvg %>%
  ungroup() %>%
  filter(gsis_pos == "QB" & RowNum > 150) %>%
  mutate(diff = totalFPgame_pred8 - totalFPgame_predroc) %>%
  # filter(diff < -1) %>%
  dplyr::select(gsis_name, gsis_pos, season, week, totalFPgame_pred8, totalFPgame_predroc, diff
                ,totalFPgame_lastx, etotalFPgame_lastx, totalFPgame_ytd, etotalFPgame_ytd, totalFPgame_roc, RowNum
  )




mean(temp$totalFPgame_roc, na.rm = TRUE)
?mean

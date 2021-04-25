library(tidyverse)
library(ggplot2)
library(mgcv)
library(broom)
# library(modelr)
library(hexbin)
library(randomForest)
library(skimr)
library(purrr)
library(ranger)

# library(pscl)
# library(lubridate)
# library(stargazer)
# library(MASS)
# library(car)
# library(DHARMa)
# library(fitdistrplus)
# library(ggpubr)
#library(RSQLite)

#setwd("~/GitHub/DynastyProcess-Apps/ep")
setwd('C:/Users/syd23/OneDrive/Documents/dynastyprocess-apps/ep')

#Import roster and game data
gsis_roster <- read_csv('C:/Users/syd23/OneDrive/Documents/dynastyprocess-apps/ep/gsis_rosters.csv')
games <- read.csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv", fileEncoding = "UTF-8-BOM")

#Functions
get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}

#Import 2009-2018 pbp data
filelist <- grep('reg_pbp', list.files(), value = TRUE) %>% .[1:10]
pbp <- data.frame()
for (f in filelist){
  df <- read.csv(f)
  pbp <- bind_rows(pbp,df)
  rm(df,f)
}

# Test Code ---------------------------------------------------------------
# research <- pbp %>%
#   dplyr::select(desc, rusher_player_name, rusher_player_id, rusher_gsis_name,
#                 receiver_player_name, receiver_player_id, receiver_gsis_name,
#                 passer_player_name, passer_player_id, passer_gsis_name) %>%
#   filter(!is.na(passer_player_name) & is.na(passer_gsis_name)) %>%
#   group_by(passer_player_name, passer_gsis_name) %>%
#   summarise(n = n())
# 
# research <- pbp %>%
#   filter(receiver_player_id  == "00-0027917") %>%
#   distinct(posteam, season)
# 
# temp <- pbp %>%
#   #filter(air_yards == 0 & play_type == "pass") %>%
#   group_by(play_type) %>%
#   summarise(n = n())
# 
# temp <- pbp_gsis %>%
#   filter(play_type == "pass" & is.na(pass_location) & sack == 0 & interception == 0 & two_point_attempt == 0) %>%
#   group_by(pass_location) %>%
#   summarise(n = n())
# 
# temp <- pbp %>%
#        filter(is.na(air_yards) & play_type == "pass" & sack == 0 & two_point_attempt == 0)
# 
# temp <- pbp %>%
#   filter(play_type == "run" & is.na(run_location) & two_point_attempt == 0 & !grepl("kneel",desc))
# unique(rushdf$run_gap)
# 
# temp <- pbp %>%
#   filter(play_type == "run" & grepl("Aborted", desc))


# Rush Data ---------------------------------------------------------------
rushdf <- pbp %>%
  left_join(games, by = "game_id") %>%
  left_join(dplyr::select(gsis_roster,
                          GSIS_ID,
                          rusher_gsis_bday = Birth_date,
                          rusher_gsis_pos = Position),
            by = c("rusher_player_id" = "GSIS_ID")) %>%
  filter(play_type == "run" & two_point_attempt == 0 & !grepl("kneel",desc) & !is.na(yards_gained) & !is.na(rusher_gsis_pos)) %>%
  mutate(rusher_age = get_age(rusher_gsis_bday, game_date, dec = TRUE),
         rushFP = 6*rush_touchdown + 0.1*yards_gained - 2*fumble_lost,
         run_gap = ifelse(is.na(run_gap), "center", as.character(run_gap)),
         run_location = ifelse(is.na(run_location), "unknown", as.character(run_location)),
         run_gap_dir = paste0(run_location,run_gap),
         run_gap_dir = case_when(run_gap_dir %in% c("leftcenter","rightcenter","middleend") | run_location == "unknown" ~ "unknown",
                                 TRUE ~ run_gap_dir)) %>%
  select(game_id, season, week, posteam_type,  rusher_player_name, rusher_gsis_pos, rusher_age,
         rushFP, yards_gained, rush_touchdown, yardline_100,
         run_gap, run_location, run_gap_dir, shotgun, ydstogo) %>%
  mutate_if(is.character, as.factor)

#Rushing Yard Models
rushYDgam <- bam(yards_gained ~ s(yardline_100, k =12) + rusher_gsis_pos + shotgun + run_gap_dir + s(rusher_age)  + s(ydstogo),
                 data=rushdf, method = "REML", select = TRUE)

rushYDrf <- ranger(yards_gained ~ yardline_100 + rusher_gsis_pos + shotgun + run_gap_dir + rusher_age + ydstogo, data = rushdf,
                   num.trees = 250,  mtry = 2, min.node.size   = 11, sample.fraction = 0.6, seed = 123, importance = 'impurity')

rushdf$eRushYDgam <- predict(rushYDgam, rushdf)
rushdf$eRushYDrf <- predict(rushYDrf, rushdf)$predictions

cor(rushdf$eRushYDgam, rushdf$eRushYDrf)

rushYDcombo<- bam(yards_gained ~ s(eRushYDgam,k=18) + s(eRushYDrf,k=25) + ti(eRushYDgam,eRushYDrf),
                  data=rushdf, method = "REML", select = TRUE)

vis.gam(rushYDcombo, view = c("eRushYDgam","eRushYDrf"), plot.type = "contour", too.far=.07)

rushdf$eRushYD = predict(rushYDcombo, rushdf)
rushdf$eRushYD = ifelse(rushdf$eRushYD > rushdf$yardline_100,rushdf$yardline_100,rushdf$eRushYD)

#Rushing TD Model
rushTDModGam <- bam(rush_touchdown ~ s(yardline_100, k =15) + s(eRushYD, k = 15) + ti(yardline_100, eRushYD),
                    data=rushdf, method = "REML", family=binomial(link="logit"), select = TRUE)
vis.gam(rushTDModGam, view = c("yardline_100","eRushYD"), plot.type = "contour", too.far=.07)

# rushTDrf <- ranger(rush_touchdown ~ yardline_100 + eRushYD, data = rushdf,
#                    num.trees = 400,  mtry = 1, min.node.size   = 11, sample.fraction = 0.5, seed = 123, importance = 'impurity')

# rushdf$eRushTDGam <- predict(rushTDModGam, rushdf, type = "response")
# rushdf$eRushTDrf <- predict(rushTDrf, rushdf, type = "response")$predictions

# rushTDcombo <- gam(rush_touchdown ~ s(eRushTDGam) + s(eRushTDrf, k=15),
#                    data=rushdf, method = "REML", family=binomial(link="logit"), select = TRUE)

rushdf$eRushTD <- predict(rushTDModGam, rushdf, type = "response")

# Model Plots -------------------------------------------------------------
rushpred %>%
  filter(yards_gained > 10) %>%
  ggplot() +
  #geom_hex(aes(yards_gained, predCombo), bins = 25) +
  geom_hex(aes(yards_gained, pred), bins = 25) +
  ylim(-5,30)+
  xlim(-5,30) +
  geom_abline(color = "red", size = 2) +
  coord_fixed() +
  facet_wrap(~rusher_gsis_pos)

rushpred %>%
  ggplot() +
  geom_density(aes(x = yards_gained), color = "black") +
  geom_density(aes(x = predGam), color = "blue") +
  #geom_density(aes(x = predTree), color = "orange")+
  geom_density(aes(x = predCombo), color = "red")+
  xlim(-5,15) +
  facet_wrap(~rusher_gsis_pos)
  
rushpred %>% 
  ggplot() +
  geom_hex(aes(yards_gained, predCombo), bins = 25)  +
  geom_abline(color = "red", size = 2) +
  coord_fixed() +
  facet_wrap(~rusher_gsis_pos)
  
rushpred %>% 
  filter(rusher_gsis_pos %in% c("QB","RB","WR") & resid < 5) %>%
  ggplot() +
  geom_hex(aes(yardline_100, resid), bins = 25) +
  facet_wrap(~rusher_gsis_pos)
  
rushpred %>%
  ggplot()+
  geom_hex(aes(x = yardline_100, y = yards_gained), bins = 25) +
  geom_smooth(aes(yardline_100, predGam), color = "red") +
  geom_smooth(aes(yardline_100, predCombo), color = "green") +
  ylim(-5,20)+
  facet_wrap(~rusher_gsis_pos)

rushpred %>%
  ggplot()+
  geom_hex(aes(x = rusher_age, y = yards_gained), bins = 25) +
  geom_smooth(aes(rusher_age, predGam), color = "red")+
  geom_smooth(aes(rusher_age, predCombo), color = "green") +
  geom_smooth(aes(rusher_age, yards_gained), color = "black")+
  ylim(-5,20)+
  facet_wrap(~rusher_gsis_pos)

# Pass Data ---------------------------------------------------------------
passdf <- pbp %>% 
  left_join(games, by = "game_id") %>%
  left_join(dplyr::select(gsis_roster,
                          GSIS_ID,
                          receiver_gsis_bday = Birth_date,
                          receiver_gsis_pos = Position),
            by = c("receiver_player_id" = "GSIS_ID")) %>%
  left_join(dplyr::select(gsis_roster,
                          GSIS_ID,
                          passer_gsis_bday = Birth_date,
                          passer_gsis_pos = Position),
            by = c("passer_player_id" = "GSIS_ID")) %>%
  filter(play_type == "pass" & sack == 0 & two_point_attempt == 0 & !is.na(air_yards)
         & !is.na(receiver_gsis_pos) & !is.na(passer_gsis_pos)) %>%
  mutate(passer_age = get_age(passer_gsis_bday, game_date, dec = TRUE),
         receiver_age = get_age(receiver_gsis_bday, game_date, dec = TRUE),
         recFP = 6*pass_touchdown + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         passFP =  4*pass_touchdown + 0.04*yards_gained - 2*fumble_lost - 2*interception,
         pass_location = ifelse(is.na(pass_location), "unknown", as.character(pass_location)),
         targetline = yardline_100 - air_yards)%>%
  select(game_id, season, week, posteam_type,  passer_player_name, passer_gsis_pos, passer_age,
         receiver_player_name, receiver_gsis_pos, receiver_age,
         passFP, recFP, yards_gained, pass_touchdown, yardline_100, air_yards,
         pass_location, targetline, shotgun, complete_pass, ydstogo)


#Reception Model
recModGam <- bam(complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20)  + as.factor(receiver_gsis_pos) + ti(air_yards,yardline_100)
                 + s(passer_age) + as.factor(pass_location),
                 data=passdf, method = "REML", family=binomial(link="logit"), select = TRUE)

recModDrf <- ranger(complete_pass ~ air_yards + yardline_100  + receiver_gsis_pos + passer_age + pass_location, data = passdf,
                   num.trees = 200,  mtry = 1, min.node.size   = 4, sample.fraction = 0.5, seed = 123, importance = 'impurity')

passdf$eRecgam <- predict(recModGam, passdf, type = "response")
passdf$eRecrf <- predict(recModDrf, passdf, type = "response")$predictions

cor(passdf$eRecgam, passdf$eRecrf)

Reccombo<- bam(complete_pass ~ s(eRecgam) + s(eRecrf) + ti(eRecgam,eRecrf),
                  data=passdf, method = "REML", family=binomial(link="logit"), select = TRUE)

vis.gam(Reccombo, view = c("eRecgam","eRecrf"), plot.type = "contour", too.far=.07)

passdf$eRec <- predict(Reccombo, passdf, type = "response")

passdf %>%
  ggplot()+
  geom_hex(aes(x = air_yards, y = eRec), bins = 25) +
  geom_smooth(aes(air_yards, complete_pass), color = "black") +
  geom_smooth(aes(air_yards, eRecgam), color = "green") +
  geom_smooth(aes(air_yards, eRecrf), color = "red") +
  geom_smooth(aes(air_yards, eRec), color = "purple") +
  xlim(0,30)+
  facet_wrap(~receiver_gsis_pos)

passdf %>%
  ggplot()+
  #geom_hex(aes(x = air_yards, y = eRec), bins = 25) +
  geom_smooth(aes(yardline_100, complete_pass), color = "black") +
  geom_smooth(aes(yardline_100, eRecgam), color = "green") +
  geom_smooth(aes(yardline_100, eRecrf), color = "red") +
  geom_smooth(aes(yardline_100, eRec), color = "purple") +
  xlim(0,100)+
  facet_wrap(~receiver_gsis_pos)

hist(recdf$pass_touchdown)

descdist(recdf$pass_touchdown, discrete = FALSE, boot=1000)
# fitbeta <- fitdist(recdf$pass_touchdown,"beta")
# plot(fitbeta)
descdist(recdf$yards_gained, discrete = FALSE, boot=1000)
descdist(recdf$complete_pass, discrete = FALSE)
descdist(recdf$first_down_pass, discrete = FALSE)

##Reception Model
recMod <- glm(complete_pass ~ logyardline + yardlinesq + abs_air_yards + pass_location + shotgun ,
              data=recdf,
              family=binomial(link="logit"))

recModGam <- gam(complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20)  + receiver_gsis_pos + ti(air_yards,yardline_100),
                 data=recdf, method = "REML", family=binomial(link="logit"), select = TRUE)

# modeldf <- rbind(modeldf,
#                  data.frame(toString(recModGam3$formula),
#                       recModGam3$aic,
#                       summary(recModGam3)$r.sq,
#                       summary(recModGam3)$dev.expl))
# 
# modeldfTemp <- modeldf %>%
#   arrange(recModGam3.aic) %>%
#   mutate(AICdiff = recModGam3.aic - lag(recModGam3.aic))

summary(recModGam)
plot(recModGam)
gam.check(recModGam)
#vis.gam(recModGam, view = c("air_yards","yardline_100"),theta=35,phi=20, too.far=.07)
#vis.gam(recModGam, view = c("air_yards","yardline_100"), plot.type = "contour", too.far=.07)

recdf$eRec <- predict(recMod, recdf, type="response")
recdf$eRecGam <- predict(recModGam, recdf, type="response")

sqrt(mean((recdf$complete_pass - recdf$eRec) ^ 2 , na.rm=TRUE))
sqrt(mean((recdf$complete_pass - recdf$eRecGam) ^ 2 , na.rm=TRUE))

explore <- recdf %>%
  group_by(yardline_100) %>% 
  summarise(
    n= n(),
    avg_rec = mean(complete_pass, na.rm=TRUE),
    avg_erec = mean(eRec, na.rm=TRUE),
    avg_erecgam = mean(eRecGam, na.rm=TRUE),
  )

ggplot(explore) + 
  geom_point(aes(x = yardline_100, y = avg_rec), color = 'blue') + 
  geom_point(aes(x = yardline_100, y = avg_erec), color = 'red') +
  geom_point(aes(x = yardline_100, y = avg_erecgam), color = 'green') +
  xlim(-10,50)

recdftemp <- filter(recdf, eRecGam > 0.8 & eRecYDGam < 4) %>%
  dplyr::select(pass_touchdown, recFP, eRecFP, eRecGam, eRecYDGam, complete_pass, yardline_100, desc, abs_air_yards, air_yards, yards_gained, pass_location, receiver_gsis_pos, 
                half_seconds_remaining, goal_to_go, game_half, shotgun, no_huddle, ydstogo, down) %>%
  summarise(mean(yards_gained))
ggplot() + 
  geom_point(aes(x = recdftemp$yardline_100, y = recdftemp$eRecGam, color = recdftemp$air_yards)) +
  geom_point(aes(x = explore$yardline_100, y = explore$avg_rec), color = 'red')

#Receiving Yards Model
recYDMod <- lm(yards_gained ~ logyardline + yardlinesq + abs_air_yards + pass_location + shotgun,
               data=recdf)

recYDModGam <- gam(yards_gained ~ s(air_yards) + s(eRecGam) + pass_location,
                   data=recdf, method = "REML", select = TRUE)

modeldf2 <- rbind(modeldf2,
                  data.frame(toString(recYDModGam$formula),
                             length(recYDModGam[["var.summary"]]),
                             recYDModGam$aic,
                             summary(recYDModGam)$r.sq,
                             summary(recYDModGam)$dev.expl))

ggplot(modeldf2, aes(x = modeldf2$length.recYDModGam...var.summary....,
                     y = modeldf2$recYDModGam.aic,
                     fill = modeldf2$toString.recYDModGam.formula.)) +
  geom_bar(stat='identity',position="dodge") + 
  coord_cartesian(ylim=c(1200000,1300000))

ggplot(modeldf2, aes(x = modeldf2$length.recYDModGam...var.summary....,
                     y = modeldf2$summary.recYDModGam..r.sq,
                     fill = modeldf2$toString.recYDModGam.formula.)) +
  geom_bar(stat='identity',position="dodge") + 
  coord_cartesian(ylim=c(0.05,0.12))

summary(recYDModGam)
plot(recYDModGam)
gam.check(recYDModGam)

recdf$eRecYD <- predict(recYDMod, recdf)
recdf$eRecYDGam <- predict(recYDModGam, recdf)
recdf$eRecYDGam = ifelse(recdf$eRecYDGam > recdf$yardline_100,recdf$yardline_100,recdf$eRecYDGam)

sqrt(mean((recdf$yards_gained - recdf$eRecYD) ^ 2 , na.rm=TRUE))
sqrt(mean((recdf$yards_gained - recdf$eRecYDGam) ^ 2 , na.rm=TRUE))
#sqrt(mean((recdf$yards_gained - recdf$eRecYDGam2) ^ 2 , na.rm=TRUE))

explore <- recdf %>%
  filter(air_yards > -5 & air_yards < 50) %>%
  group_by(yardline_100) %>% 
  summarise(
    n= n(),
    avg_recYD = mean(yards_gained, na.rm=TRUE),
    avg_erecYD = mean(eRecYD, na.rm=TRUE),
    avg_erecYDgam = mean(eRecYDGam, na.rm=TRUE),
  )

ggplot(explore) + 
  geom_point(aes(x = yardline_100, y = avg_recYD), color = 'blue') + 
  geom_point(aes(x = yardline_100, y = avg_erecYD), color = 'red') +
  geom_point(aes(x = yardline_100, y = avg_erecYDgam), color = 'green') +
  xlim(-10,50)


recdftemp <- filter(recdf, air_yards > -5 & air_yards < 50)
ggplot() + 
  geom_point(aes(x = recdftemp$yardline_100, y = recdftemp$eRecYDGam, color = recdftemp$air_yards)) +
  geom_point(aes(x = explore$yardline_100, y = explore$avg_recYD), color = 'red') 


temp <- filter(recdf, eRecYDGam == yardline_100) %>%
  dplyr::select(pass_touchdown, eRecYDGam, yardline_100, desc, abs_air_yards, air_yards, yards_gained, pass_location, receiver_gsis_pos, 
                half_seconds_remaining, goal_to_go, game_half, shotgun, no_huddle, ydstogo, down)

#Receiving TD Model
recTDMod <- glm(pass_touchdown ~ yardlinesq + yardline_100 + abs_air_yards + pass_location,
                data=recdf,
                family=binomial(link="logit"))
recTDModGam <- gam(pass_touchdown ~ s(yardline_100, k=15) + s(eRecGam, k=15) + s(eRecYDGam, k=15),
                   data=recdf,
                   method = "REML",
                   family=binomial(link="logit"),
                   select = TRUE
                   )

modeldf2 <- rbind(modeldf2,
                 data.frame(toString(recTDModGam$formula),
                            recTDModGam$aic,
                            summary(recTDModGam)$r.sq,
                            summary(recTDModGam)$dev.expl))

AIC(recTDMod, recTDModGam)
summary(recTDModGam)
plot(recTDModGam)
gam.check(recTDModGam)

recdf$eRecTD <- predict(recTDMod, recdf, type="response")
recdf$eRecTDGam <- predict(recTDModGam, recdf, type="response")

sqrt(mean((recdf$pass_touchdown - recdf$eRecTD) ^ 2 , na.rm=TRUE))
sqrt(mean((recdf$pass_touchdown - recdf$eRecTDGam) ^ 2 , na.rm=TRUE))

explore <- recdf %>%
  group_by(yardline_100) %>% 
  summarise(
    n = n(),
    avg_td = mean(pass_touchdown, na.rm=TRUE),
    avg_etd = mean(eRecTD, na.rm=TRUE),
    avg_etdgam = mean(eRecTDGam, na.rm=TRUE),
  )

ggplot(explore) + 
  geom_point(aes(x = yardline_100, y = avg_td), color = 'blue') + 
  geom_point(aes(x = yardline_100, y = avg_etd), color = 'red') +
  geom_point(aes(x = yardline_100, y = avg_etdgam), color = 'green')

ggplot() + 
  geom_point(aes(x = recdf$yardline_100, y = recdf$eRecTDGam, color = recdf$abs_air_yards)) +
  geom_point(aes(x = explore$yardline_100, y = explore$avg_td), color = 'red')

temp <- filter(recdf, eRecTDGam > 0.5) %>%
  dplyr::select(pass_touchdown, eRecTDGam, yardline_100, desc, abs_air_yards, air_yards,  pass_location, receiver_gsis_pos, 
           half_seconds_remaining, goal_to_go, game_half, shotgun, no_huddle, ydstogo, down) %>%
  summarise(mean(pass_touchdown))

#Interception model
intModGam <- gam(interception ~ s(air_yards) + s(eRecGam),
                   data=recdf,
                   method = "REML",
                   family=binomial(link="logit"),
                   select = TRUE)
recdf$eIntGam <- predict(intModGam, recdf, type="response")

summary(intModGam)
plot(intModGam)
gam.check(intModGam)

explore <- recdf %>%
  filter(air_yards < 50 & air_yards > -5) %>%
  group_by(air_yards) %>% 
  summarise(
    n = n(),
    avg_td = mean(interception, na.rm=TRUE),
    avg_etd = mean(eIntGam, na.rm=TRUE),
    avg_etdgam = mean(eIntGam, na.rm=TRUE),
  )

ggplot(explore) + 
  geom_point(aes(x = air_yards, y = avg_td), color = 'blue') + 
  geom_point(aes(x = air_yards, y = avg_etd), color = 'red') +
  geom_point(aes(x = air_yards, y = avg_etdgam), color = 'green')

ggplot() + 
  geom_point(aes(x = recdf$yardline_100, y = recdf$eIntGam, color = recdf$air_yards)) +
  geom_point(aes(x = explore$yardline_100, y = explore$avg_td), color = 'red')


#First Down Model

rec1DMod <- glm(first_down_pass ~ logyardline + yardline_100 + abs_air_yards + pass_location + shotgun + ydstogo + factor(down),
                data=recdf,
                family=binomial(link="logit"))
rec1DModGam <- gam(first_down_pass ~ logyardline + yardline_100 + abs_air_yards + pass_location + shotgun + ydstogo + factor(down),
                data=recdf,
                family=binomial(link="logit"))

#Predictions
recdf$eRecYD <- predict(recYDMod, recdf)
recdf$eRec <- plogis(predict(recMod, recdf))
recdf$eRec1D <- plogis(predict(rec1DMod, recdf))


#Receiving FP Model
recFPMod <- lm(recFP ~ eRecTD + eRecYD + eRec , data=recdf)
recFPModGam <- gam(recFP ~ s(eRecTDGam, k = 30) + s(eRecYDGam, k = 15) + s(eRecGam, k = 20), data=recdf)
recFPModGam2 <- gam(recFP ~ s(eRecTDGam, k = 30) + s(eRecYDGam, k = 20) +
                      s(eRecGam, k = 20) + ti(eRecGam, eRecYDGam, k = 8), data=recdf)
recFPModGam3 <- gam(recFP ~ s(eRecTDGam, k = 30) + s(eRecYDGam, k = 20) +
                      s(eRecGam, k = 20) + ti(eRecGam, eRecYDGam, k = 9) +
                      ti(eRecGam, eRecTDGam, k = 6), data=recdf)
#AIC(recFPMod, recFPModGam)
AIC(recFPModGam2, recFPModGam)

summary(recFPMod)
summary(recFPModGam)
summary(recFPModGam2)
summary(recFPModGam3)

plot(recFPModGam3)
gam.check(recFPModGam3)
#vis.gam(recFPModGam2, view = c("eRecGam","eRecYDGam"),theta=35,phi=20, too.far=.07)
#vis.gam(recFPModGam3, view = c("eRecGam","eRecYDGam"), plot.type = "contour", too.far=.07)
#vis.gam(recFPModGam3, view = c("eRecGam","eRecTDGam"), plot.type = "contour", too.far=.05)
recdf$eRecFP <- predict(recFPModGam3, recdf)

#Passing FP Model
passFPModGam <- gam(passFP ~ s(eRecTDGam, k = 35) + s(eRecYDGam, k = 20) + s(eRecGam, k = 25), data=recdf)
passFPModGam2 <- gam(passFP ~ s(eRecTDGam, k = 35) + s(eRecYDGam, k = 25) +
                      s(eRecGam, k = 20) + ti(eRecGam, eRecYDGam, k = 10), data=recdf)
passFPModGam3 <- gam(passFP ~ s(eRecTDGam, k = 35) + s(eRecYDGam, k = 20) + s(eRecGam, k = 25) +
                      ti(eRecGam, eRecYDGam, k = 10) + ti(eRecGam, eRecTDGam, k = 8),
                    data=recdf, method = "REML", select = TRUE)

summary(passFPModGam)
summary(passFPModGam2)
summary(passFPModGam3)

plot(passFPModGam3)
gam.check(passFPModGam3)
#vis.gam(passFPModGam3, view = c("eRecGam","eRecYDGam"),theta=35,phi=20, too.far=.07)
#vis.gam(passFPModGam3, view = c("eRecGam","eRecYDGam"), plot.type = "contour", too.far=.07)
#vis.gam(passFPModGam3, view = c("eRecGam","eRecTDGam"), plot.type = "contour", too.far=.05)

#Other Models
recFP1DMod <- lm(recFP1D ~ eRecTD + eRecYD + eRec1D, data=recdf)
#stepAIC(recFPMod)
#stargazer(recFPMod, type = "text")
recdf$eRecFP <- predict(recFPMod, recdf)

#Concordance(recdf$recFP, recdf$eFP)
#Concordance(recdf$yards_gained, recdf$eYD)

#Rush YD Model

# rushdf %>%
#   group_by(run_gap_dir) %>%
#   summarise(n = n())
# ggboxplot(rushdf, x = "run_gap_dir", y= "yards_gained",
#           color = "run_gap_dir",
#           order = c("leftend","lefttackle","leftguard","middlecenter","rightguard","righttackle","rightend","unknown"),
#           ylim = c(-5,25))
# 
# anov <- aov(yards_gained ~ run_location, data = rushdf)
# summary(anov)
# TukeyHSD(anov)
# 
# temp <- filter(rushdf, is.na(yards_gained)) %>% dplyr::select(desc, run_gap, run_location, qb_kneel, fumble)
# temp <- filter(rushdf, grepl("kneel",desc)) %>% dplyr::select(desc, run_gap, run_location, qb_kneel, play_type)
temp <- rushdf19 %>%
  group_by(rusher_gsis_name) %>%
  summarise(total = sum(eRushYd, na.rm=TRUE))

rushYDMod <- lm(yards_gained ~ logyardline + yardline_100 + factor(down) + shotgun + run_gap,
                data=rushdf)

rushYDModGam <- gam(yards_gained ~ s(yardline_100) + rusher_gsis_pos + shotgun + run_gap_dir,
                   data=rushdf, method = "REML", select = TRUE)

modeldf2 <- rbind(modeldf2,
                  data.frame(toString(rushYDModGam$formula),
                             length(rushYDModGam[["var.summary"]]),
                             rushYDModGam$aic,
                             summary(rushYDModGam)$r.sq,
                             summary(rushYDModGam)$dev.expl))

ggplot(modeldf2, aes(x = modeldf2$length.rushYDModGam...var.summary....,
                     y = modeldf2$rushYDModGam.aic,
                     fill = modeldf2$toString.rushYDModGam.formula.)) +
  geom_bar(stat='identity',position="dodge") + 
  coord_cartesian(ylim=c(800000,900000))

ggplot(modeldf2, aes(x = modeldf2$length.rushYDModGam...var.summary....,
                     y = modeldf2$summary.rushYDModGam..r.sq,
                     fill = modeldf2$toString.rushYDModGam.formula.)) +
  geom_bar(stat='identity',position="dodge") + 
  coord_cartesian(ylim=c(0,0.04))

summary(rushYDModGam)
plot(rushYDModGam)
gam.check(rushYDModGam)

rushdf$eRushYD <- predict(rushYDMod, rushdf)
rushdf$eRushYDGam <- predict(rushYDModGam, rushdf)
rushdf$eRushYDGam = ifelse(rushdf$eRushYDGam > rushdf$yardline_100,rushdf$yardline_100,rushdf$eRushYDGam)

sqrt(mean((rushdf$yards_gained - rushdf$eRushYD) ^ 2 , na.rm=TRUE))
sqrt(mean((rushdf$yards_gained - rushdf$eRushYDGam) ^ 2 , na.rm=TRUE))
#sqrt(mean((recdf$yards_gained - recdf$eRecYDGam2) ^ 2 , na.rm=TRUE))

explore <- rushdf %>%
  group_by(yardline_100) %>% 
  summarise(
    n= n(),
    avg_recYD = mean(yards_gained, na.rm=TRUE),
    avg_erecYD = mean(eRushYD, na.rm=TRUE),
    avg_erecYDgam = mean(eRushYDGam, na.rm=TRUE),
  )

ggplot(explore) + 
  geom_point(aes(x = yardline_100, y = avg_recYD), color = 'blue') + 
  geom_point(aes(x = yardline_100, y = avg_erecYD), color = 'red') +
  geom_point(aes(x = yardline_100, y = avg_erecYDgam), color = 'green')


ggplot() + 
  geom_point(aes(x = rushdf$yardline_100, y = rushdf$eRushYDGam), color="blue") +
  geom_point(aes(x = explore$yardline_100, y = explore$avg_recYD), color = 'red') 

temp <- filter(rushdf, eRushYDGam < -2) %>% dplyr::select(yards_gained, desc, yardline_100, shotgun, run_gap_dir)

#Rush TD Model
rushTDMod <- glm(rush_touchdown ~ logyardline + yardline_100 + factor(down) + shotgun + run_gap
                 , data=rushdf, family=binomial(link="logit"))

rushTDModGam <- gam(rush_touchdown ~ s(yardline_100) + s(eRushYDGam),
                    data=rushdf, method = "REML", select = TRUE, family=binomial(link="logit"))

modeldf2 <- rbind(modeldf2,
                  data.frame(toString(rushTDModGam$formula),
                             length(rushTDModGam[["var.summary"]]),
                             rushTDModGam$aic,
                             summary(rushTDModGam)$r.sq,
                             summary(rushTDModGam)$dev.expl))

ggplot(modeldf2, aes(x = modeldf2$length.rushTDModGam...var.summary....,
                     y = modeldf2$rushTDModGam.aic,
                     fill = modeldf2$toString.rushTDModGam.formula.)) +
  geom_bar(stat='identity',position="dodge") + 
  coord_cartesian(ylim=c(20000,30000))

ggplot(modeldf2, aes(x = modeldf2$length.rushTDModGam...var.summary....,
                     y = modeldf2$summary.rushTDModGam..r.sq,
                     fill = modeldf2$toString.rushTDModGam.formula.)) +
  geom_bar(stat='identity',position="dodge") + 
  coord_cartesian(ylim=c(0.2,0.3))

summary(rushTDModGam)
plot(rushTDModGam)
gam.check(rushTDModGam)

rushdf$eRushTD <- predict(rushTDMod, rushdf, type = "response")
rushdf$eRushTDGam <- predict(rushTDModGam, rushdf, type = "response")

sqrt(mean((rushdf$rush_touchdown - rushdf$eRushTD) ^ 2 , na.rm=TRUE))
sqrt(mean((rushdf$rush_touchdown - rushdf$eRushTDGam) ^ 2 , na.rm=TRUE))
#sqrt(mean((recdf$yards_gained - recdf$eRecYDGam2) ^ 2 , na.rm=TRUE))

explore <- rushdf %>%
  group_by(yardline_100) %>% 
  summarise(
    n= n(),
    avg_recYD = mean(rush_touchdown, na.rm=TRUE),
    avg_erecYD = mean(eRushTD, na.rm=TRUE),
    avg_erecYDgam = mean(eRushTDGam, na.rm=TRUE),
  )

ggplot(explore) + 
  geom_point(aes(x = yardline_100, y = avg_recYD), color = 'blue') + 
  geom_point(aes(x = yardline_100, y = avg_erecYD), color = 'red') +
  geom_point(aes(x = yardline_100, y = avg_erecYDgam), color = 'green')


ggplot() + 
  geom_point(aes(x = rushdf$yardline_100, y = rushdf$eRushTDGam), color="blue") +
  geom_point(aes(x = explore$yardline_100, y = explore$avg_recYD), color = 'red')

temp <- filter(rushdf, eRushYDGam < -2) %>% dplyr::select(yards_gained, desc, yardline_100, shotgun, run_gap_dir)

#Rushing FP Model
rushFPMod <- lm(rushFP ~ eRushTD + eRushYD , data=rushdf)
rushFPModGam <- gam(rushFP ~ s(eRushTDGam, k = 15) + s(eRushYDGam, k = 15), data=rushdf)
rushFPModGam2 <- gam(rushFP ~ s(eRushTDGam, k = 20) + s(eRushYDGam, k = 20), data=rushdf)
rushFPModGam3 <- gam(rushFP ~ s(eRushTDGam, k = 20) + s(eRushYDGam, k = 20) + ti(eRushTDGam,eRushYDGam, k = 8), data=rushdf)

#AIC(rushFPMod, rushFPModGam)
AIC(rushFPModGam2, rushFPModGam)

summary(rushFPMod)
summary(rushFPModGam)
summary(rushFPModGam2)
summary(rushFPModGam3)
plot(rushFPModGam2)
gam.check(rushFPModGam2)


#vis.gam(rushFPModGam3, view = c("eRushTDGam","eRushYDGam"), plot.type = "contour", too.far=.1)

#Other models

#rushFBMod <- glm(fumble ~ yardline_100 + factor(down) + run_gap2 , data=rushdf, family=binomial(link="logit"))
rush1DMod <- glm(first_down_rush ~ logyardline + yardline_100 + factor(down) + shotgun + run_gap2 + shotgun + ydstogo, data=rushdf, family=binomial(link="logit"))

#stepAIC(rush1dMod)
#vif(rush1dMod)
#stargazer(rushYDMod, type = "text")
#pR2(rush1dMod)

rushdf$eRushTD <- plogis(predict(rushTDMod, rushdf))
#rushdf$eFB <- plogis(predict(rushFBMod, rushdf))
rushdf$eRushYD <- predict(rushYDMod, rushdf)
rushdf$eRush1D <- plogis(predict(rush1DMod, rushdf))

rushFPMod <- lm(rushFP ~ eRushTD + eRushYD, data=rushdf)
rushFP1DMod <- lm(rushFP1D ~ eRushTD + eRushYD + eRush1D, data=rushdf)

#stargazer(rushFP1DMod, type = "text")
#Concordance(rushdf$rush_touchdown, rushdf$eTD)
#Concordance(rushdf$yards_gained, rushdf$eYD)
rushdf$eRushFP <- predict(rushFPMod, rushdf)
rushdf$eRushFP1d <- predict(rushFP1DMod, rushdf)

#df <- bind_rows(recdf, rushdf)

recTDMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recYDMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rec1DMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recFPMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
recFP1DMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL

rushTDMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rushYDMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rush1DMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rushFPMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL
rushFP1DMod[c("residuals","effects","fitted.values","model","linear.predictors","weights","prior.weights","y","data")] <- NULL

save(recTDMod, recYDMod, recMod, rec1DMod, recFPMod, recFP1DMod,
     rushTDMod, rushYDMod, rush1DMod, rushFPMod, rushFP1DMod, file = "models.rda")

dfsum <- df %>%
  group_by(game_id, drive) %>%
  summarise(eTD = sum(eTD, na.rm=TRUE),
            eYD = sum(eYD, na.rm=TRUE),
            maxYds = max(yardline_100)) %>%
  filter(eYD > maxYds)



recMod <- lm(recFP ~ logyardline + yardline_100 + factor(down)
             + abs_air_yards + shotgun  + pass_location, data=rectrainingData)  # build the model
stepAIC(recMod)
stargazer(recMod, type = "text")

rushMod <- lm(rushFP ~ logyardline + yardlinesq + yardline_100 + factor(down)
              + shotgun + run_gap2 , data=rushtrainingData)  # build the model
stepAIC(rushMod)
stargazer(rushMod, type = "text")

save(recMod, rushMod, file = "models.rda")


rectestData$recEP <- predict(lmMod, rectestData)
rushtestData$rushEP <- predict(lmMod2, rushtestData)

sqrt(mean((rectestData$recFP - rectestData$recEP) ^ 2 , na.rm=TRUE))
sqrt(mean((rushtestData$rushFP - rushtestData$rushEP) ^ 2 , na.rm=TRUE))

ggplot(rectestData) + 
  geom_point(aes(x = recEP, y = recFP), color = 'red')


ggplot(rushdf) + 
  geom_point(aes(x = yardline_100, y = first_down_rush), color = 'red')

ggplot(rushtestData) + 
  geom_point(aes(x = rushEP, y = rushFP), color = 'red')


rectrainingData$recEP <- predict(lmMod, rectrainingData)
rushtrainingData$rushEP <- predict(lmMod2, rushtrainingData)


explore <- recdf %>%
  
  explore <- rectrainingData %>%
  
  #select(play_type, yards_gained, air_yards, yardline_100, ydstogo, recFP, recEP) %>%
  group_by(yardline_100) %>% 
  summarise(
    #avg_EP = mean(recEP, na.rm = TRUE),
    #avg_FP = mean(recFP, na.rm = TRUE),
    avg_1d = mean(first_down_pass, na.rm = TRUE))

ggplot(explore) + 
  geom_point(aes(x = yardline_100, y = avg_1d), color = 'red') + 
  geom_point(aes(x = yardline_100, y = avg_FP), color = 'green')

exploreay <- rectrainingData %>%
  #select(play_type, yards_gained, air_yards, yardline_100, ydstogo, recFP, recEP) %>%
  group_by(air_yards) %>% 
  summarise(
    avg_EP = mean(recEP, na.rm = TRUE),
    avg_FP = mean(recFP, na.rm = TRUE))

ggplot(exploreay) + 
  geom_point(aes(x = air_yards, y = avg_EP), color = 'red') + 
  geom_point(aes(x = air_yards, y = avg_FP), color = 'blue') +
  xlim(-5,45) +
  ylim(0,5)

explore2 <- rushtrainingData %>%
  #select(play_type, yards_gained, air_yards, yardline_100, ydstogo, rushFP, rushEP, down) %>%
  group_by(yardline_100) %>% 
  summarise(
    avg_EP = mean(rushEP, na.rm = TRUE),
    avg_FP = mean(rushFP, na.rm = TRUE),
    avg_TD = mean(6*touchdown, na.rm = TRUE))

ggplot(explore2) + 
  geom_point(aes(x = yardline_100, y = avg_EP), color = 'red') + 
  geom_point(aes(x = yardline_100, y = avg_FP), color = 'green') +
  geom_point(aes(x = yardline_100, y = avg_TD), color = 'blue')


recdf2019$recEP <- predict(lmMod, recdf2019)
rushdf2019$rushEP <- predict(lmMod2, rushdf2019)

teamrecEP <- recdf2019 %>%
  group_by(posteam) %>%
  summarise(TeamTargets = sum(pass_attempt),
            TeamCatches = sum(complete_pass),
            TeamAYs = sum(abs_air_yards, na.rm=TRUE),
            TeamRecEP = sum(recEP, na.rm=TRUE)
  )

playerrushEP <- rushdf2019 %>%
  group_by(rusher_player_name) %>%
  summarise(rushEP = sum(rushEP, na.rm=TRUE),
            rushFP = format(sum(rushFP), scientific = FALSE),
            diff = format(as.numeric(rushFP) - rushEP, nsmall=1),
            Rushes = sum(rush_attempt))

playerrecEP <- recdf2019 %>%
  filter(!is.na(receiver_player_name)) %>%
  group_by(receiver_player_name, posteam) %>%
  summarise(recEP = sum(recEP, na.rm=TRUE),
            recFP = format(sum(recFP), scientific = FALSE),
            diff = format(as.numeric(recFP) - recEP, nsmall=1),
            Targets = sum(pass_attempt),
            Catches = sum(complete_pass),
            AYs = sum(air_yards),
            recYds = sum(yards_gained),
            aDOT = mean(air_yards)
  ) %>%
  inner_join(teamrecEP, by = "posteam") %>%
  mutate(AYshare = AYs / TeamAYs,
         TargetShare = Targets / TeamTargets,
         WOPR = 1.4*TargetShare + 0.7*AYshare,
         RACR = recYds / AYs,
         YPTPA = recYds / TeamTargets) %>%
  dplyr::select(receiver_player_name, posteam, recEP, recFP, diff, Targets, Catches, recYds, AYshare, TargetShare, aDOT, WOPR, RACR, YPTPA)




temp <- rushdf2019 %>%
  dplyr::select(rusher_player_name, yardline_100, rushEP, rushFP) %>%
  filter(rusher_player_name == "M.Mack")

temp2 <- recdf2019 %>%
  dplyr::select(receiver_player_name, yardline_100, air_yards, recEP, recFP) %>%
  filter(receiver_player_name == "T.Kelce")

temp2 <- recdf2019 %>%
  #dplyr::select(receiver_player_name, yardline_100, recEP, rechFP) %>%
  filter(is.na(receiver_player_name) & sack != 1)


temp2 <- recdf %>%
  #dplyr::select(receiver_player_name, yardline_100, recEP, rechFP) %>%
  filter( air_yards <= -15)

temp2 <- df2019 %>%
  group_by(pass_defense_1_player_name) %>%
  summarise(n())
filter( air_yards <= -15)
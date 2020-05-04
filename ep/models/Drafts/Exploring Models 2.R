# Libraries ----------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(here)
library(arrow)
library(mgcv)
library(ggrepel)

library(furrr)
library(vip)
library(skimr)

# Setup ----------------------------------------------------------------
set.seed(1234)
memory.limit(size=30000)
setwd(here())
plan(multisession)
rosters <- read_parquet("data/rosters/rosters_1999_2019.pdata")
pbp <- read_parquet("data/pbp_data/pbp_reg_post_1999_2019.pdata")

# Functions ---------------------------------------------------------------
get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}

# Rush Data ---------------------------------------------------------------
rushdf <- pbp %>%
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          rusher_gsis_bday = Birth_date,
                          rusher_gsis_pos = Position),
            by = c("rusher_player_id" = "GSIS_ID")) %>%
  filter(play_type == "run",
         #two_point_attempt == 0,
         !grepl("kneel",desc),
         !is.na(yards_gained),
         !is.na(rusher_gsis_pos),
         !is.na(alt_game_id)) %>%
  mutate(rusher_age = get_age(rusher_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         wind_speed = case_when(wind >= 15 ~ "high",
                                wind < 15 ~ "low",
                                TRUE ~ "unknown"),
         rushFP = 6*rush_touchdown  + 2*two_point_converted + 0.1*yards_gained - 2*fumble_lost,
         score = ifelse(rush_touchdown == 1 | two_point_conv_result == "success", 1, 0),
         run_gap = ifelse(is.na(run_gap), "center", as.character(run_gap)),
         run_location = ifelse(is.na(run_location), "unknown", as.character(run_location)),
         run_gap_dir = paste0(run_location,run_gap),
         run_gap_dir = case_when(run_gap_dir %in% c("leftcenter","rightcenter","middleend") | run_location == "unknown" ~ "unknown",
                                 TRUE ~ run_gap_dir)) %>%
  select(season, week, posteam_type, rusher_gsis_pos, rusher_age,
         rushFP, yards_gained, rush_touchdown, yardline_100, two_point_attempt,
         run_gap, run_location, run_gap_dir, qb_scramble,
         no_huddle, shotgun, ydstogo,
         wind_speed, roof, surface, wp) %>%
  mutate_if(is.character, as.factor)
  #na.omit()

# Pass Data ---------------------------------------------------------------
passdf <- pbp %>% 
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          receiver_gsis_bday = Birth_date,
                          receiver_gsis_pos = Position),
            by = c("receiver_player_id" = "GSIS_ID")) %>%
  left_join(dplyr::select(rosters,
                          GSIS_ID,
                          passer_gsis_bday = Birth_date,
                          passer_gsis_pos = Position),
            by = c("passer_player_id" = "GSIS_ID")) %>%
  filter(play_type == "pass",
         sack == 0,
         season >= 2006,
         #two_point_attempt == 0,
         !is.na(air_yards),
         !is.na(receiver_gsis_pos),
         !is.na(passer_gsis_pos)) %>%
  mutate(passer_age = get_age(passer_gsis_bday, game_date, dec = TRUE),
         receiver_age = get_age(receiver_gsis_bday, game_date, dec = TRUE),
         two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                         is.na(two_point_conv_result) & grepl("ATTEMPT SUCCEEDS", desc) ~ 1,
                                         TRUE ~ 0),
         wind_speed = case_when(wind >= 15 ~ "high",
                                wind < 15 ~ "low",
                                TRUE ~ "unknown"),
         wind = case_when(roof == "closed" | roof == "dome" ~ 0,
                          is.na(wind) ~ 8,
                          TRUE ~ wind),
         recFP = 6*pass_touchdown + 2*two_point_converted  + 0.1*yards_gained - 2*fumble_lost + complete_pass,
         passFP =  4*pass_touchdown + 2*two_point_converted  + 0.04*yards_gained - 2*fumble_lost - 2*interception,
         score = ifelse(rush_touchdown == 1 | two_point_conv_result == "success", 1, 0),
         pass_location = ifelse(is.na(pass_location), "unknown", as.character(pass_location)),
         targetline = yardline_100 - air_yards)%>%
  select(game_id, season, week, posteam_type, passer_gsis_pos, passer_age,
         receiver_gsis_pos, receiver_age, two_point_attempt, down,
         passFP, recFP, yards_gained, pass_touchdown, yardline_100, air_yards,
         pass_location, targetline, shotgun, complete_pass, ydstogo,
         no_huddle, wind_speed, wind, roof, surface, wp)%>%
  mutate_if(is.character, as.factor)
#na.omit()

rm(pbp)

# Train Test Split Data ---------------------------------------------------
rushdf_split <- initial_split(rushdf, prop = 4/5, strata = season)

rushdf_train <- training(rushdf_split)
rushdf_test <- testing(rushdf_split)

passdf_split <- initial_split(passdf, prop = 4/5, strata = season)

passdf_train <- training(passdf_split)
passdf_test <- testing(passdf_split)


# Functions ---------------------------------------------------------------

fit_bam <- function(form) {
  bam(as.formula(form), data=passdf_train, method = "REML", family=binomial(link="logit"), select = TRUE )
}

bam_rsq <- function(mod) {
  summary(mod)[["r.sq"]]
}

bam_aic <- function(mod) {
  mod[["aic"]]
}

#Reception Model
passdf_train %>%
  filter(is.na(wind)) %>%
  group_by(roof) %>%
  summarise(avg = mean(wind, na.rm= TRUE),
            count = n())


passdf_train %>%
  ggplot() +
  geom_smooth(aes(receiver_age, complete_pass), color = "red") +
  geom_smooth(aes(passer_age, complete_pass), color = "green")
  
models <- tibble(model_name = c("recModGam1","recModGam2","recModGam3","recModGam4","recModGam5","recModGam6","recModGam7","recModGam8","recModGam9","recMod10"),
                 formulas = c("complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos",
                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location",
                              #"complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(down)",
                              
                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(passer_age)",
                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(passer_age)
                                               + ti(passer_age,air_yards)",
                              
                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(wind)",
                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(wind) +
                                               ti(air_yards,wind)",
                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(wind) + s(passer_age) +
                                               ti(passer_age,wind)",
                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(wind) + s(passer_age) +
                                               ti(air_yards,wind) + ti(passer_age,wind)",

                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(ydstogo) + 
                                               ti(ydstogo,air_yards)",
                              "complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(ydstogo) + 
                                               ti(ydstogo,air_yards) + ti(ydstogo, yardline_100)"
                              )
                 )

# models <- tibble(model_name = c("recModGam1","recModGam2","recModGam3","recModGam4","recModGam5"),
#                  model_spec = list(recModGam1, recModGam2, recModGam3, recModGam4, recModGam5))

model_fits <- models %>%
  mutate(fit = future_map(formulas, fit_bam, .progress = TRUE),
         RSQ = future_map(fit, bam_rsq),
         AIC = future_map(fit, bam_aic)) %>%
  unnest(c("RSQ", "AIC")) %>%
  mutate(terms = str_count(formulas, "\\+") + 1,
         summs = future_map(fit,summary))

model_fits %>%
  ggplot(aes(terms, RSQ)) +
  geom_point(aes(size= AIC))+
  geom_label_repel(aes(label = model_name))

int <- 5
model_fits$summs[[int]]
plot(model_fits$fit[[int]])

vis.gam(model_fits$fit[[9]], view = c("ydstogo","yardline_100"), plot.type = "contour", too.far=.02)

temp <- bam(complete_pass ~ s(air_yards, k=30) + s(yardline_100, k=20) + ti(air_yards,yardline_100) + receiver_gsis_pos + pass_location + s(wind) + s(passer_age) +
                                               ti(air_yards,wind) + ti(passer_age,wind),
            data=passdf_train, method = "REML", family=binomial(link="logit"), select = TRUE)
            

passdf_train$eRecGam <- predict(recModGam, passdf_train, type="response")

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
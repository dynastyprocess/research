library(ffscrapr)
library(tidyverse)
library(purrr)
library(here)
library(skimr)


fp_scrape <- read_csv("fp_scrapes_week0_2012_2020.csv") %>%
  #filter(season == 2019) %>%
  mutate(mfl_id = as.character(mfl_id)) %>% 
  select(-pos)

league_id <- 60206

get_yearlydata <- function(year,week){
  
  conn <- mfl_connect(year, league_id, user_agent = "dynastyprocess", rate_limit_number = 30, rate_limit_seconds = 60)
  
  players <- mfl_players(conn)
  
  mfl_getendpoint(conn, "playerScores", W=week, YEAR=year, RULES=1) %>% 
    purrr::pluck("content","playerScores","playerScore") %>% 
    tibble() %>% 
    unnest_wider(1) %>%
    left_join(players, by = c("id" = "player_id")) %>%
    mutate(pos_category = case_when(pos %in% c("QB","RB","WR","TE","TMQB","TMRB","TMWR","TMTE","Off") ~ "off",
                                    pos %in% c("DT","DE","LB","S","CB","TMDL","TMLB","TMDB") ~ "def",
                                    pos %in% c("PK","PN","Def","ST","Coach","TMPK","TMPN") ~ "off",
                                    TRUE ~ "ERROR"),
           score = as.numeric(score)) %>% 
    select(id, pos, score)
  
}

nested_ppg <- crossing(year = 2016:2019,
                       week = 1:17) %>%
  mutate(playerscore = map2(year,week,get_yearlydata)) %>%
  unnest(playerscore) %>%
  group_by(year, id, pos) %>%
  summarise(ppg = round(mean(score, na.rm = TRUE),2),
            games = n()) %>%
  ungroup() %>% 
  left_join(fp_scrape, by = c("id" = "mfl_id", "year" = "season"))

not_all_na <- function(x) any(!is.na(x))

df <- nested_ppg %>% 
  filter(pos == "WR", !is.na(fan_pts_mean), year <2019) %>% 
  #select(ppg, ends_with("mean"), -starts_with("rec"), -starts_with("fmb")) %>% 
  select(-id, -pos, -games, -fantasypros_id, -player_name, -starts_with("pass")) %>% 
  select_if(not_all_na) %>% 
  replace(is.na(.), 0)


df2 <- nested_ppg %>% 
  filter(pos == "WR", !is.na(fan_pts_mean), year == 2019) %>% 
  #select(ppg, ends_with("mean"), -starts_with("rec"), -starts_with("fmb")) %>% 
  #select(-id, -pos, -games, -fantasypros_id, -player_name, -starts_with("rec")) %>% 
  select_if(not_all_na) %>% 
  replace(is.na(.), 0)

#Regression Models
library(GGally)

ggpairs(df)

#Stepwise
library(leaps)

lm1 <- lm(ppg ~ ., data = df)
lm_step <- step(lm1, direction = "backward")
summary(lm_step)

lm2 <- lm(ppg ~ 1, data = df)

lm_stepf <- step(lm2,
                 scope = ppg ~ fan_pts_mean + fan_pts_max + fan_pts_min + pass_att_mean,
                 direction = "forward")

lmexh <- summary(regsubsets(ppg ~ ., data = df))

best <- paste0("ppg~",
               paste(names(which(lmexh$which[which.min(lmexh$bic),] == TRUE))[-1],
                     collapse = "+"))

lm_best <- lm(best, data = df)
summary(lm_best)

df2$ppgpredict <- predict(lm_best, df2)
df2 <- df2 %>%  mutate(fpppg = fan_pts_mean/16) %>% relocate(ppgpredict, fpppg) 

library(ggpubr)

df2 %>%
  ggscatter(x = "ppg",
            y = "ppgpredict",
            add = "reg.line",
            conf.int = TRUE, 
            #cor.coef = TRUE,
            cor.method = "pearson") +
  # facet_wrap(~pos, scales = "free") +
  labs(title = "FantasySharks Correlations by Position ADL 2019") +
  theme_bw() +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.x = 1
  )

df2 %>%
  ggscatter(x = "ppg",
            y = "fpppg",
            add = "reg.line",
            conf.int = TRUE, 
            #cor.coef = TRUE,
            cor.method = "pearson") +
  # facet_wrap(~pos, scales = "free") +
  labs(title = "FantasySharks Correlations by Position ADL 2019") +
  theme_bw() +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.x = 1
  )

#Data Checks
fp_scrape %>% 
  filter(pos == "qb", pass_att_mean >=300) %>% 
  group_by(season) %>% 
  tally()

fp_scrape %>% 
  filter(pos == "te", rec_mean >= 45) %>% 
  group_by(season) %>% 
  tally()

fp_scrape %>% 
  filter(pos == "rb", rush_att_mean >= 75) %>% 
  group_by(season) %>% 
  tally()
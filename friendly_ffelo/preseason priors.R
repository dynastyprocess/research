library(ffscrapr)
library(tidyverse)
library(here)

setwd(here())

fp_scrape <- read_csv("fp_scrapes_week0_2012_2020.csv") %>%
  #filter(season == 2019) %>%
  mutate(mfl_id = as.character(mfl_id),
         fp_ppg = fan_pts_mean/16) %>% 
  select(-pos, -player_name)

league_id <- 60206

get_weeklyscore <- function(year, week){
  
  conn <- mfl_connect(year, league_id, user_agent = "dynastyprocess", rate_limit_number = 30, rate_limit_seconds = 60)
  
  players <- mfl_players(conn)
  
  projections <- mfl_getendpoint(conn, "projectedScores", W=week, YEAR=year) %>%
    pluck("content","projectedScores","playerScore") %>%
    tibble() %>% 
    unnest_wider(1) %>%
    rename(player_id = id, projected_score = score)
  
  mfl_getendpoint(conn, "weeklyResults", W=week, YEAR=year) %>% 
    pluck("content","weeklyResults","matchup") %>% 
    tibble() %>% 
    unnest_wider(1) %>%
    unnest_longer("franchise") %>%
    unnest_wider("franchise") %>% 
    unnest_longer("player") %>%
    unnest_wider("player",names_sep = "_") %>%
    left_join(players, by = c("player_id")) %>%
    left_join(projections, by = c("player_id")) %>%
    mutate(pos_category = case_when(pos %in% c("QB","RB","WR","TE","TMQB","TMRB","TMWR","TMTE","Off") ~ "off",
                                    pos %in% c("DT","DE","LB","S","CB","TMDL","TMLB","TMDB") ~ "def",
                                    pos %in% c("PK","PN","Def","ST","Coach","TMPK","TMPN") ~ "off",
                                    TRUE ~ "ERROR"),
           player_score = as.numeric(player_score),
           projected_score = as.numeric(projected_score)) 
}

get_standings <- function(year) {
  conn <- mfl_connect(year, league_id, user_agent = "dynastyprocess", rate_limit_number = 30, rate_limit_seconds = 60)
  
  standings <- ffscrapr::ff_standings(conn) %>% 
    select(franchise_id, h2h_wins, h2h_winpct, allplay_wins, allplay_winpct, points_for, potential_points)
  
}

weekly_df <- crossing(year = 2017:2019,
                      week = 1:1) %>%
  mutate(week_scores = map2(year,week,get_weeklyscore)) %>%
  unnest("week_scores") %>% 
  left_join(fp_scrape, by = c("player_id" = "mfl_id", "year" = "season")) %>% 
  select(id, year, player_name, pos, pos_category, player_status, projected_score, fp_ppg)

standings <- tibble(year = 2017:2019) %>% 
  mutate(yearly_standings = map(year,get_standings)) %>%
  unnest("yearly_standings")
  
team_df <- weekly_df %>% 
  group_by(id, year, pos_category, player_status) %>% 
  summarise(week1_projection = sum(projected_score, na.rm = TRUE),
            fp_ppg_projection = sum(fp_ppg, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(id, year),
              names_from = c(pos_category, player_status),
              values_from = c(week1_projection, fp_ppg_projection)) %>% 
  inner_join(standings, by = c("id" = "franchise_id", "year"))

library(GGally)

df <- team_df %>% 
  select(h2h_wins, allplay_wins, points_for, potential_points,
         starts_with("week1"), starts_with("fp_ppg"), -fp_ppg_projection_def_nonstarter,
         -fp_ppg_projection_def_starter, -fp_ppg_projection_ERROR_nonstarter, -week1_projection_ERROR_nonstarter)

ggpairs(df)


library(leaps)

lm1 <- lm(h2h_wins ~ week1_projection_def_nonstarter + week1_projection_def_starter
          + week1_projection_off_nonstarter + week1_projection_off_starter
          + fp_ppg_projection_def_nonstarter + fp_ppg_projection_def_starter 
          + fp_ppg_projection_off_nonstarter + fp_ppg_projection_off_starter,
          data = team_df)

lmexh <- summary(regsubsets(h2h_wins ~ week1_projection_def_nonstarter + week1_projection_def_starter
                            + week1_projection_off_nonstarter + week1_projection_off_starter
                            + fp_ppg_projection_def_nonstarter + fp_ppg_projection_def_starter 
                            + fp_ppg_projection_off_nonstarter + fp_ppg_projection_off_starter,
                            data = team_df))

best <- paste0("h2h_wins~",
               paste(names(which(lmexh$which[which.min(lmexh$bic),] == TRUE))[-1],
                     collapse = "+"))

lm_best <- lm(best, data = team_df)
summary(lm_best)

team_df$winspred <- predict(lm_best, team_df)
team_df <- team_df %>% relocate(winspred) 


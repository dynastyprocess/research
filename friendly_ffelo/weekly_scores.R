library(ffscrapr)
library(tidyverse)
library(here)

setwd(here())

league_id <- 22627

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
           projected_score = as.numeric(projected_score)) %>% 
    group_by(id, pos_category, score, opt_pts, result) %>%
    summarise(total_points = sum(if_else(player_status == "starter", player_score, 0), na.rm = TRUE),
              opt_points = sum(if_else(player_shouldStart == 1, player_score, 0), na.rm = TRUE),
              starter_projection = sum(if_else(player_status == "starter", projected_score, 0), na.rm = TRUE)) %>%
    ungroup() %>% 
    pivot_wider(id_cols = c("id", "score", "opt_pts", "result"),
                names_from = pos_category,
                values_from = c(total_points, opt_points, starter_projection),
                names_glue = "{pos_category}_{.value}") %>%
    mutate(check_total = off_total_points + def_total_points,
           check_opt = off_opt_points + def_opt_points,
           all_play_wins = rank(score)-1,
           all_play_games = n()-1)
}

weekly_df <- crossing(year = 2019:2019,
                       week = 1:4) %>%
  mutate(week_scores = map2(year,week,get_weeklyscore)) %>%
  unnest("week_scores")


#validation
temp <- df %>%
  filter(id == "0011") %>%
  select(player_name, pos, score, shouldStart, status.x)

temp <- df %>%
  filter(d == "0004") %>%
  select(player_name, pos, score, shouldStart, status.x)

#Correlation Plot
library(ggpubr)
df %>%
  mutate(player_score = if_else(is.na(player_score),0,player_score),
         pos = factor(pos, levels = c("QB","RB","WR","TE","DT","DE","LB","S","CB"))) %>% 
  ggscatter(x = "player_score",
            y = "projected_score",
            add = "reg.line",
            conf.int = TRUE, 
            #cor.coef = TRUE,
            cor.method = "pearson") +
  facet_wrap(~pos, scales = "free") +
  labs(title = "FantasySharks Correlations by Position Week 8, 2019") +
  theme_bw() +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.x = 3
  )

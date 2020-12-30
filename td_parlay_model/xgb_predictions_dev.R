suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(slider)
  library(lubridate)
  #library(glue)
  #library(magrittr)
  
  # Plotting
  library(ggbeeswarm)
  library(ggthemes)
  library(directlabels)
  library(ggrepel)
  
  #EDA
  library(skimr)
  library(usemodels)
  library(tidymodels)
  library(vip)
  library(baguette)
})

# Import Data -------------------------------------------------------------
setwd(here::here())
memory.limit(60000)

con <- DBI::dbConnect(odbc::odbc(), "dynastyprocess")
dk <- dbGetQuery(con, "Select * from dk_playerprops_td where week = 16")

ecr_pos_past <- dbGetQuery(con, "SELECT sportsdata_id as sportradar_id, ecr as ecr_pos_combo, scrape_date
                            from fp_ecr
                            where page_type in ('weekly-qb','weekly-rb','weekly-wr','weekly-te')") %>%
  filter(scrape_date != max(scrape_date)) %>%
  filter(scrape_date == max(scrape_date)) %>%
  
  select(-scrape_date)

ecr_ovr_past <- dbGetQuery(con, "SELECT sportsdata_id as sportradar_id, ecr as ecr_ovr_combo, scrape_date
                            from fp_ecr where page_type in ('weekly-op')") %>%
  filter(scrape_date != max(scrape_date)) %>%
  filter(scrape_date == max(scrape_date)) %>%
  
  select(-scrape_date)

dbDisconnect(con)
rm(con)

# Game Spreads ------------------------------------------------------------

games <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")

games_away <- games %>%
  select(game_id, week, season, gameday, posteam = away_team, spread_line, total_line, result) %>%
  mutate(posteam_type = "away",
         implied_total = if_else(spread_line<=0, (total_line+spread_line)/2 - spread_line, (total_line-spread_line)/2))

games_home <- games %>%
  select(game_id, week, season, gameday, posteam = home_team, spread_line, total_line, result) %>%
  mutate(posteam_type = "home",
         spread_line = -spread_line,
         implied_total = if_else(spread_line<=0, (total_line+spread_line)/2 - spread_line, (total_line-spread_line)/2))

games_combined <-
  bind_rows(games_away, games_home) %>%
  arrange(game_id) %>%
  group_by(posteam) %>%
  mutate(Team = case_when(posteam == "ARZ" ~ "ARI",
                          posteam == "BLT" ~ "BAL",
                          posteam == "CLV" ~ "CLE",
                          posteam == "HST" ~ "HOU",
                          posteam == "JAC" ~ "JAX",
                          posteam == "LA" ~ "LAR",
                          posteam == "STL" ~ "LAR",
                          posteam == "SAN" ~ "LAC",
                          posteam == "SD" ~ "LAC",
                          posteam == "SL" ~ "LAR",

                          TRUE ~ posteam),
         across(.cols = c(posteam_type, spread_line, total_line, implied_total),
                .fns = ~lead(.x),
                .names = "{.col}_next"),
         last_gameday = max(if_else(!is.na(result),gameday,as_date('1990-1-1')))) %>%
  ungroup() %>%
  filter(gameday == last_gameday, season == year(today())) %>%
  select(Team, spread_line_next, total_line_next, implied_total_next, posteam_type_next)


# Predict 2020 data -------------------------------------------------------
load("parlay_pred_models_updated.rda")

ep_feature_mart <- read_arrow("C:/Users/syd23/Documents/DynastyProcess/db/feature_mart.pdata")

#Predict Next Week
prep_2020 <- ep_feature_mart %>%
  group_by(gsis_id) %>% 
  filter(sum(Season == 2020) > 0, gsis_game_id == max(gsis_game_id)) %>%
  ungroup() %>% 
  select(-(contains("next") & -ends_with("to_next")), -ecr_ovr_combo, -ecr_pos_combo) %>%
  left_join(games_combined, by = c("Team" = "Team")) %>% 
  left_join(ecr_pos_past, by = c("sportradar_id")) %>% 
  left_join(ecr_ovr_past, by = c("sportradar_id")) %>%
  left_join(ecr_pos, by = c("sportradar_id")) %>% 
  left_join(ecr_ovr, by = c("sportradar_id")) %>% 
  mutate(pos_group = case_when(Pos %in% c('WR','TE') ~ 'Rec',
                               TRUE ~ Pos)) %>% 
  arrange(gsis_game_id, Team, pos_group, ecr_ovr_combo_next) %>% 
  group_by(gsis_game_id, Team, pos_group) %>%
  mutate(teammate_ecr_rank_next = row_number(),
         teammate_ecr_gap_to_better_next = lag(ecr_ovr_combo_next) - ecr_ovr_combo_next,
         teammate_ecr_gap_to_next_next = lead(ecr_ovr_combo_next) - ecr_ovr_combo_next,
         teammate_ecr_gap_to_best_next = min(ecr_ovr_combo_next) - ecr_ovr_combo_next,
         across(contains("teammate"), ~replace_na(.x, 0)),
         across(contains("teammate"), ~round(.x, 2)),
         Week = as.numeric(Week),
         week_group = as.factor(case_when(Week > 17 ~ "Playoffs",
                                          TRUE ~ as.character(Week)))) %>% 
  ungroup()

insert_mergename <- . %>%
  mutate(merge_name = Name) %>% 
  mutate_at('merge_name',str_remove_all,"( Jr)|( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('merge_name',str_squish) %>%
  mutate_at('merge_name',tolower)

te_predictions <- prep_2020 %>%
  filter(Pos == "TE") %>% 
  bind_cols(predict(te_td_pred, .))

qb_predictions <- prep_2020 %>%
  filter(Pos == "QB") %>% 
  bind_cols(predict(qb_td_pred, .))

rb_predictions <- prep_2020 %>%
  filter(Pos == "RB") %>% 
  mutate(.pred = 0)
  bind_cols(predict(rb_td_pred, .))

wr_predictions <- prep_2020 %>%
  filter(Pos == "WR") %>% 
  bind_cols(predict(wr_td_pred, .))

parlay_predictions_2020 <- #qb_predictions %>% 
  rbind(rb_predictions) %>% 
  rbind(wr_predictions) %>% 
  #rbind(te_predictions) %>% 
  insert_mergename() %>% 
  select(Name, Team, Week, Pos, merge_name,
         .pred, ecr_ovr_combo_next, ecr_ovr_combo,
         ecr_pos_combo_next, ecr_pos_combo,
         total_fp_roll10, total_fp_share_roll10,
         rush_fp_roll9, rush_fp_x_share_career, rush_fp_x_share_roll3,
         pass_att_roll15)

bet_today <- dk %>% 
  mutate(merge_name = case_when(name == "Josh Allen (Bills)" ~ "josh allen",
                                name == "Michael Thomas (Saints)" ~ "michael thomas",
                                name == "Michael Pittman Jr" ~ "michael pittman",
                                name == "Jeff Wilson" ~ "jeffery wilson",
                                name == "Benny Snell Jr" ~ "benny snell",
                                name == "N’Keal Harry" ~ "nkeal harry",
                                name == "Wilie Snead" ~ "willie snead",
                                name == "Scotty Miller" ~ "scott miller",
                                name == "Greg Ward Jr" ~ "greg ward",
                                TRUE ~ merge_name)) %>% 
  left_join(parlay_predictions_2020, by = "merge_name") %>% 
  select(name, merge_name, Team, Week, Pos, ecr_ovr_combo_next, implied_odds_first_td, implied_odds_any_td, .pred) %>% 
  mutate(diff = implied_odds_any_td - .pred,
         across(where(is.numeric), round, 3)) %>% 
  group_by(Pos) %>% 
  mutate(pos_pred_rank = row_number(-.pred),
         pos_odds_rank = row_number(-implied_odds_any_td)) %>% 
  ungroup()

error_check <- bet_today %>% 
  filter(is.na(.pred))

bet_filter <- bet_today %>%
  filter(Pos == "WR") %>% 
  select(name, Team, Week, Pos, merge_name,
         .pred, ecr_ovr_combo_next,
         ecr_pos_combo_next, ecr_pos_combo,
         total_fp_share_roll10)

bet_filter <- bet_today %>%
  filter(diff <= -0.05, ecr_ovr_combo_next <= 200)


bet_filter <- bet_today %>%
  filter(diff <= -0.05, ecr_ovr_combo_next <= 250)

dk_dfs <- bet_today %>%
  filter(!(Team %in% c('GB','CAR','BUF','DEN','NYG','CLE','PIT','CIN','LAC','LV')))

#Bet Slate
bet_today %>% 
  #filter(Team %in% c("SEA","PHI")) %>% 
  filter(Pos %in% c("WR"), ecr_ovr_combo_next <= 200) %>%
  #filter(.pred >= 0.3) %>%
  #filter(.pred >= 0.2, .pred >= implied_odds_any_td) %>% 
  ggplot(aes(x=.pred, y=implied_odds_any_td, label = name), color = Pos) +
  geom_point() +
  geom_label_repel() + 
  geom_smooth(color = "red", se = FALSE, method = "lm") +
  geom_abline() +
  theme_minimal()

#Bet Game
bet_today %>% 
  #filter(Pos == "TE") %>%
  #filter(Team == "HOU") %>% 
  #filter(Team %in% c("GB","CAR","BUF","DEN")) %>%
  filter(Team %in% c("NYG","CLE")) %>% 
  ggplot(aes(x=.pred, y=implied_odds_any_td, label = name), color = Pos) +
  geom_point() +
  geom_label_repel() + 
  geom_smooth(color = "red", se = FALSE, method = "lm") +
  geom_abline() +
  labs(x = "Predicted TD", y = "DK TD Odds") +
  theme_minimal() 

#Bet First
bet_today %>%
  #mutate(implied_odds_first_td = if_else(name == "Davante Adams", 0.0909, implied_odds_first_td)) %>% 
  filter(Team %in% c("NYG","CLE")) %>% 
  #filter((.pred >= 0.2 & .pred >= implied_odds_any_td) | name == "Davante Adams")  %>% 
  #filter(.pred >= 0.2, .pred >= implied_odds_any_td) %>% 
  ggplot(aes(x=implied_odds_first_td, y= (.pred * 0.234195) - 0.003587, label = name), color = Pos) +
  geom_point() +
  geom_label_repel() + 
  geom_smooth(color = "red", se = FALSE, method = "lm") +
  labs(x = "DK Odds 1st TD", y = "Model Adjust 1st TD") +
  geom_abline() +
  theme_minimal() 

#roster check

roster <- parlay_predictions_2020 %>% filter(Name %in% c("Jeffery Wilson", "D.J. Moore", "J.K. Dobbins"))

roster <- parlay_predictions_2020 %>% filter(Name %in% c('Russell Gage',"Malcolm Brown","Samaje Perine","Jeremy McNichols","Dion Lewis","Collin Johnson","James Washington","Demarcus Robinson","Gerald Everett", "Jimmy Graham", "Irv Smith"))

roster <- parlay_predictions_2020 %>% filter(Name %in% c("Lynn Bowden","Darnell Mooney","Jared Cook","Noah Fant"))

roster <- parlay_predictions_2020 %>% filter(Name %in% c("Corey Davis","Dallas Goedert","JuJu Smith-Schuster","Chris Godwin","Jeffery Wilson"))

bet_today %>%
  #filter(Team %in% c("TEN","GB")) %>% 
  filter(ecr_ovr_combo_next <= 200) %>%
  #filter(ecr_ovr_combo_next <= 300) %>% 
  ggplot(aes(x=ecr_ovr_combo_next, y= implied_odds_any_td, label = name), color = Pos) +
  geom_point() +
  geom_label_repel() + 
  geom_smooth(color = "red", se = FALSE, method = "lm") +
  theme_minimal() 

ep_feature_mart %>%
  filter(ecr_pos_combo <=10) %>% 
  #filter(Name == "Allen Robinson", Season == "2020") %>% 
  group_by(parlay_td) %>% 
  tally()

temp <- ep_feature_mart %>% select(-c(contains("roll") | contains("career") | contains("season")))

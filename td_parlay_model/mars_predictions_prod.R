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

con <- DBI::dbConnect(odbc::odbc(), "dynastyprocess")
dk <- dbGetQuery(con, "Select * from dk_playerprops_td where week = 18")

ecr_pos <- dbGetQuery(con, "SELECT sportsdata_id as sportradar_id, ecr as ecr_pos_pred_next, scrape_date
                            from fp_ecr
                            where page_type in ('weekly-qb','weekly-rb','weekly-wr','weekly-te')") %>%
  filter(scrape_date == max(scrape_date)) %>%
  select(-scrape_date)

ecr_ovr <- dbGetQuery(con, "SELECT sportsdata_id as sportradar_id, ecr as ecr_ovr_pred_next, scrape_date
                            from fp_ecr where page_type in ('weekly-op')") %>%
  filter(scrape_date == max(scrape_date)) %>%
  select(-scrape_date)

dbDisconnect(con)
rm(con)

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
  filter(gameday == last_gameday, season == 2020) %>% 
  select(Team, spread_line_next, total_line_next, implied_total_next, posteam_type_next)
  # mutate(spread_line_next = case_when(Team == "DET" ~ -2,
  #                                     Team == "CAR" ~ 2,
  #                                     TRUE~spread_line_next),
  #        total_line_next = case_when(Team == "DET" ~ 46.5,
  #                                    Team == "CAR" ~ 46.5,
  #                                    TRUE~total_line_next),
  #        implied_total_next = case_when(Team == "DET" ~ 24.25,
  #                                       Team == "CAR" ~ 22.25,
  #                                       TRUE~implied_total_next))

# Predict 2020 data -------------------------------------------------------
ep_lagged_lines <- read_arrow("model_roll.pdata")
load("parlay_pred_models.rda")

#Predict Next Week
# ecr_pos <- ecr %>%
#   select(sportradar_id = sportsdata_id, ecr_pos_pred_next = ecr, scrape_date, page_type) %>% 
#   filter(page_type %in% c('weekly-qb','weekly-rb','weekly-wr','weekly-te')) %>%
#   filter(scrape_date == max(scrape_date)) %>% 
#   select(-scrape_date, -page_type)
# 
# ecr_ovr <- ecr %>%
#   select(sportradar_id = sportsdata_id, ecr_ovr_pred_next = ecr, scrape_date, page_type) %>% 
#   filter(page_type %in% c('weekly-op')) %>%
#   filter(scrape_date == max(scrape_date)) %>% 
#   select(-scrape_date, -page_type)


prep_2020 <- ep_lagged_lines %>%
  group_by(gsis_id) %>% 
  filter(sum(Season == 2020) > 0, gsis_game_id == max(gsis_game_id)) %>%
  ungroup() %>% 
  select(-c(spread_line_next, posteam_type_next, implied_total_next,
            total_line_next, parlay_td_next, ecr_ovr, ecr_pos)) %>% 
  left_join(games_combined, by = c("Team" = "Team")) %>% 
  left_join(ecr_pos, by = c("sportradar_id")) %>% 
  left_join(ecr_ovr, by = c("sportradar_id")) %>%
  filter(!is.na(ecr_ovr_pred_next),!is.na(ecr_pos_pred_next),!is.na(total_line_next))

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
  bind_cols(predict(rb_td_pred, .))


wr_predictions <- prep_2020 %>%
  filter(Pos == "WR") %>% 
  bind_cols(predict(wr_td_pred, .))

parlay_predictions_2020 <- qb_predictions %>% 
  rbind(rb_predictions) %>% 
  rbind(wr_predictions) %>% 
  rbind(te_predictions) %>% 
  insert_mergename() %>% 
  select(Name, Team, Week, merge_name, Pos, ecr_ovr_pred_next, .pred)

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
  select(name, merge_name, Team, Week, Pos, ecr_ovr_pred_next, implied_odds_any_td, .pred, implied_odds_first_td) %>% 
  mutate(diff = implied_odds_any_td - .pred,
         across(where(is.numeric), round, 3)) %>% 
  group_by(Pos) %>% 
  mutate(pos_pred_rank = row_number(-.pred),
         pos_odds_rank = row_number(-implied_odds_any_td)) %>% 
  ungroup()

error_check <- bet_today %>% 
  filter(is.na(.pred))

bet_filter <- bet_today %>%
  filter(diff <= -0.05, ecr_ovr_pred_next <= 250)


bet_filter <- bet_today %>%
  filter(diff <= 0.0, ecr_ovr_pred_next <= 50)

dk_dfs <- bet_today %>%
  filter((Team %in% c("BUF","NE","GB","TEN")))

#Bet Slate
bet_today %>% 
  #filter(Team %in% c("SEA","PHI")) %>% 
  #filter(Pos == "WR") %>%
  #filter(.pred >= 0.25) %>%
  filter(.pred >= 0.2, .pred >= implied_odds_any_td) %>% 
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
  #filter(Team %in% c("SF","ARI","MIA","LV")) %>%
  filter(Team %in% c("NE","BUF","GB","TEN")) %>% 
  ggplot(aes(x=.pred, y=implied_odds_any_td, label = name), color = Pos) +
  geom_point() +
  geom_label_repel() + 
  geom_smooth(color = "red", se = FALSE, method = "lm") +
  geom_abline() +
  labs(x = "Predicted TD", y = "DK TD Odds") +
  theme_minimal() 

#Bet First
bet_today %>%
  mutate(implied_odds_first_td = if_else(name == "Davante Adams", 0.0909, implied_odds_first_td)) %>% 
  filter(Team %in% c("NE","BUF")) %>% 
  #filter((.pred >= 0.2 & .pred >= implied_odds_any_td) | name == "Davante Adams")  %>% 
 # filter(.pred >= 0.2, .pred >= implied_odds_any_td) %>% 
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
  #filter(Team %in% c("SEA","PHI")) %>% 
  filter(Pos == "WR") %>%
  #filter(.pred >= 0.25) %>%
  #filter(.pred >= 0.2, .pred >= implied_odds_any_td) %>% 
  ggplot(aes(x=ecr_ovr_pred_next, y=implied_odds_any_td, label = name), color = Pos) +
  geom_point() +
  geom_label_repel() + 
  geom_smooth(color = "red", se = FALSE, method = "lm") +
  theme_minimal()

dk %>%
  filter(implied_odds_any_td>=.2) %>% 
  ggplot(aes(x=implied_odds_first_td, y=implied_odds_any_td, label = name)) +
  geom_point() +
  geom_label_repel() + 
  geom_smooth(color = "red", se = FALSE, method = "lm") +
  theme_minimal()



# Exploring PCAs ----------------------------------------------------------
te_bake <- bake(pull_workflow_prepped_recipe(te_td_pred),
                new_data =  prep_2020 %>% filter(Pos == "TE"))

te_bake %>%
  ggplot(aes(PC01, PC02)) +
  geom_point()
  geom_text(check_overlap = TRUE, family = "IBMPlexSans")

te_prep <- prep(pull_workflow_prepped_recipe(te_td_pred), retain = TRUE)
te_pca <- tidy(,2)

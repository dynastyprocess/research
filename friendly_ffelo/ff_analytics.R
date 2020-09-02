library(ffscrapr)
library(tidyverse)
library(purrr)
library(here)
library(ffanalytics)
library(skimr)


# Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")
# remotes::install_github("FantasyFootballAnalytics/ffanalytics", upgrade = FALSE, R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# remotes::install_github("tanho63/ffanalytics",branch = "patch-scrapedata", upgrade=FALSE)
# remotes::install_local("C:\\Users\\syd23\\Desktop\\ffanalytics", force = TRUE) 

mfl_ids <- read_csv("https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_playerids.csv") %>%
  filter(mfl_id != "11399", mfl_id != "11613", position %in% c("QB","RB","WR","TE")) %>%
  select("mfl_id", "merge_name")

# conn <- mfl_connect(2019, 60206, user_agent = "dynastyprocess", rate_limit_number = 30, rate_limit_seconds = 60)

#scoring <- ff_scoring(conn)

off_proj <- scrape_data(
  src = c("CBS", "FantasyPros", "FantasySharks", "FFToday","NumberFire"),
  pos = c("QB", "RB", "WR", "TE"),
  season = 2019,
  week = 0
) %>%
  tibble() %>% 
  unnest(1)

off_proj <- scrape_data(
  src = c("FantasyPros"),
  pos = c("QB", "RB", "WR", "TE"),
  season = 2018,
  week = 1
) %>%
  tibble() %>% 
  unnest(1)

off_proj_clean <- off_proj %>%
  tibble() %>% 
  unnest(1) %>%
  mutate_at("player",str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('player',str_squish) %>%
  mutate_at('player',tolower) %>%
  left_join(mfl_ids, by = c("player" = "merge_name")) %>%
  mutate(id = as.character(ifelse(is.na(id), mfl_id, id))) %>%
  select("data_src", "id",
         "pass_att", "pass_comp", "pass_yds", "pass_tds", "pass_int", "sacks",
         "rush_att", "rush_yds", "rush_tds", "fumbles_lost",
         "rec_tgt", "rec_yds", "rec", "rec_tds") %>% 
  pivot_wider(id_cols = id, values_from = where(is.numeric), names_from = data_src, values_fn = min)

  # group_by(player) %>%
  # tally()

# temp <- off_proj_clean %>%
#   full_join(off_proj_clean2, by = c("player")) %>%
#   filter(n.x != n.y)

  #group_by(id) %>%
  #summarise(across(c(ends_with(c("att","yds","tds")), "pass_comp", "pass_comp"), ~mean(.x, na.rm = TRUE))


# def_proj <- scrape_data(
#   src = c("FantasySharks", "FFToday"),
#   pos = c("DL", "LB", "DB"),
#   season = year,
#   week = 0
# )

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

nested_ppg <- crossing(year = 2019:2019,
                       week = 1:17) %>%
  mutate(playerscore = map2(year,week,get_yearlydata)) %>%
  unnest(playerscore) %>%
  group_by(year, id, pos) %>%
  summarise(ppg = round(mean(score, na.rm = TRUE),2),
            games = n()) %>%
  ungroup() %>% 
  left_join(off_proj_clean, by = c("id"))

not_all_na <- function(x) any(!is.na(x))

temp <- nested_ppg %>%
  filter(pos == "QB", !is.na(pass_att_NumberFire)) %>%
  select(ppg, ends_with(c("FantasyPros","NumberFire"))) %>% 
  # select(-year, -id, -games, -pos) %>% 
  select_if(not_all_na)

temp <- nested_ppg %>%
  filter(pos == "QB") %>%
  #select(ppg, ends_with(c("FantasyPros","NumberFire"))) %>% 
  # select(-year, -id, -games, -pos) %>% 
  select_if(not_all_na)

skim(temp)


lm1 <- lm(ppg ~ ., data = temp, na.action=na.omit)
summary(lm1)

options(error=recover)

view(temp %>% filter(is.na(pass_att_FantasySharks)))

temp <- df %>%
  group_by(data_src, position) %>%
  tally()


view(df %>% filter(id == "13733"))
unique(scrape_2020$DL$data_src)
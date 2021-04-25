suppressPackageStartupMessages({
  library(rvest)
  library(tidyverse)
  library(janitor)
  library(DBI)
  library(here)
  library(magrittr)
  
  options(stringsAsFactors = FALSE)
  options(scipen = 999)
  
  setwd(here())
})

insert_mergename <- . %>%
  mutate(merge_name = name) %>% 
  mutate_at('merge_name',str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('merge_name',str_squish) %>%
  mutate_at('merge_name',tolower)

dk_url <- "https://sportsbook.draftkings.com/leagues/football/3?category=player-props&subcategory=game"

dk_html <- read_html(dk_url) %>% 
  html_nodes(".scorer-7__body")

player_nodes <- dk_html %>% 
  html_nodes(".scorer-7__player") %>% 
  html_text()

any_td_props <- dk_html %>% 
  html_nodes(".scorer-7__cell~ .scorer-7__cell+ .scorer-7__cell .default-color") %>% 
  html_text()

first_td_props <- dk_html %>% 
  html_nodes(".scorer-7__cell:nth-child(1) .default-color") %>% 
  html_text()

last_td_props <- dk_html %>% 
  html_nodes(".scorer-7__cell:nth-child(2) .default-color") %>% 
  html_text()

dk_odds <- 
  tibble(name = player_nodes,
         american_odds_any_td = any_td_props,
         american_odds_first_td = first_td_props,
         american_odds_last_td = last_td_props) %>% 
  mutate(across(.cols = contains("american_odds"),
                .fns = as.numeric),
         #american_odds_td = if_else(name == "N’Keal Harry", 510, american_odds_td),
         decimal_odds_any_td = if_else(american_odds_any_td < 0, 1-100/american_odds_any_td, 1+american_odds_any_td/100),
         implied_odds_any_td = 1/decimal_odds_any_td,
         decimal_odds_first_td = if_else(american_odds_first_td < 0, 1-100/american_odds_first_td, 1+american_odds_first_td/100),
         implied_odds_first_td = 1/decimal_odds_first_td,    
         decimal_odds_last_td = if_else(american_odds_last_td < 0, 1-100/american_odds_last_td, 1+american_odds_last_td/100),
         implied_odds_last_td = 1/decimal_odds_last_td, 
         across(where(is.numeric), round, 4),
         scrape_date = Sys.Date(),
         week = 18,
         season = 2020) %>% 
  insert_mergename()

# old_dk <- dbGetQuery(aws_db, "Select * From dk_playerprops_td where week = 16") %>%
#   filter(!(name %in% c("CJ Prosise","Danny Amendola","Gerald Everett"))) %>% 
#   mutate(scrape_date = Sys.Date())
# 
# combo_dk <- dk_odds %>%
#   bind_rows(old_dk) %>%
#   distinct()
# 
# dup_ck <- combo_dk %>%
#   group_by(name) %>%
#   tally() %>%
#   filter(n >1)
 
aws_db <- dbConnect(odbc::odbc(),"dynastyprocess")
# dbExecute(aws_db, "delete from dk_playerprops_td where week = 16")
dbExecute(aws_db, "update dk_playerprops_td set week = 18 where scrape_date = '2021-01-09'")

# temp <- dbGetQuery(aws_db, "select * from dk_playerprops_td") %>% 
#   add_column(week = 11) %>% 
#   add_column(season = 2020) %>% 
#   rbind(dk_odds)
# dbRemoveTable(aws_db, "dk_playerprops_td")
# dbCreateTable(aws_db, "dk_playerprops_td", combo_dk)
dbAppendTable(aws_db,"dk_playerprops_td", dk_odds)
dbDisconnect(aws_db)


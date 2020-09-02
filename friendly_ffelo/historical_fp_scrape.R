library(rvest)
library(glue)
library(tidyverse)
library(purrr)
library(here)

setwd(here())

id_map <- read_csv("https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_playerids.csv") %>%
  filter(mfl_id != "11399", mfl_id != "11613", position %in% c("QB","RB","WR","TE","PK")) %>%
  select(fantasypros_id, mfl_id, merge_name) %>% 
  mutate(merge_name = case_when(merge_name == "mitchell trubisky" ~ "mitch trubisky",
                                merge_name == "mike badgley" ~ "michael badgley",
                                TRUE~ merge_name)) 

get_projections <- function(year, pos){
  
  if(pos == "qb"){
    col_names <- c("scrap","pass_att","pass_cmp","pass_yds","pass_tds","pass_int",
                   "rush_att","rush_yds","rush_tds","fmb_lost","fan_pts")
  } else if(pos == 'flex') {
    col_names <-  c("scrap","rush_att","rush_yds","rush_tds",
                    "rec","rec_yds","rec_tds","fmb_lost","fan_pts")
  } else if (pos == 'k'){ 
    col_names <-c("scrap","fg_made","fg_att","xpt_made","fan_pts")
  } else if (pos == 'dst') {
    col_names <- c("scrap","dst_sack","dst_int","dst_fr","dst_ff","dst_td","dst_saf","dst_pa","dst_ydsagn","fan_pts")}
  
table_node<- glue('https://www.fantasypros.com/nfl/projections/',pos,'.php?max-yes=true&min-yes=true&scoring=PPR&week=0&year=',year) %>%
  read_html() %>% 
  html_node('table')

player_id_nodes <- table_node %>% 
  html_nodes(".player-label") %>% 
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "fp-player-link", " " ))]')

player_ids <- tibble(
  player_id = html_attr(player_id_nodes,"class") %>% str_extract("[0-9]+$"),
  player_name = html_attr(player_id_nodes,"fp-player-name")
)


if(!nrow(player_ids)>0){return(tibble())}

if (pos != "flex") {
  player_pos <- tibble(pos, .rows = nrow(player_ids)) %>% rename(player_pos = pos)
  } else if(pos == "flex") {

  player_pos <- table_node %>%
    html_nodes(".player-label+ td") %>%
    html_text()
  }

max_cells <- table_node %>% 
  html_nodes(".max-cell") %>% 
  html_text() %>% 
  matrix(nrow = nrow(player_ids),
         byrow = TRUE,
         dimnames = list(1:nrow(player_ids),
                         paste(col_names,"max",sep="_"))
  )

min_cells <- table_node %>% 
  html_nodes(".min-cell") %>% 
  html_text(trim = TRUE) %>% 
  matrix(nrow = nrow(player_ids),
         byrow = TRUE,
         dimnames = list(1:nrow(player_ids),
                         paste(col_names,"min",sep="_"))
  )

xml_remove(table_node %>% html_nodes("div")) #remove min/max div tags

mean_cells <- table_node %>%
  html_nodes(".center") %>% 
  html_text() %>% 
  matrix(nrow = nrow(player_ids),
         byrow = TRUE,
         dimnames = list(1:nrow(player_ids),
                         paste(col_names[-1],"mean",sep="_"))
  )

cbind(player_ids, player_pos, mean_cells, max_cells, min_cells) %>%
  select(-starts_with("scrap"))

}


df <- tibble()

for (i in (2012:2020)){ 
  for (j in c("qb","flex","k","dst")){
    
    df <- get_projections(i,j) %>%
      mutate(season = i) %>% 
      bind_rows(df)
  }
}

df_cleaned <- df %>%
  mutate(pos = str_to_lower(str_remove(player_pos,"\\d{1,3}")),
         #across(where(is.factor), ~as.numeric(as.character(.x))),
         across(where(is.factor), ~parse_number(as.character(.x))),
         player_id = as.double(player_id),
         merge_name = player_name) %>%
  mutate_at("merge_name",str_remove_all,"( Jr.)|( Sr.)|( III)|( II)|( IV)|(\\')|(\\.)")%>%
  mutate_at('merge_name',str_squish) %>%
  mutate_at('merge_name',tolower) %>%
  filter(pos %in% c('qb','rb','wr','te','k','dst')) %>%
  left_join(id_map, by = c("merge_name"="merge_name")) %>% 
  relocate(season, mfl_id, fantasypros_id, pos, player_name,
           starts_with("fan"),
           starts_with("pass"),
           starts_with("rush"),
           starts_with("fmb"),
           starts_with("rec")) %>% 
  select(-merge_name, -player_pos, -player_id) %>% 
  arrange(season, -fan_pts_mean)

write_csv(df_cleaned, "fp_scrapes_week0_2012_2020.csv")

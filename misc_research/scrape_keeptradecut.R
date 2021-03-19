library(rvest)
library(tidymodels)
library(stringr)

ktc <-
  read_html("https://keeptradecut.com/dynasty-rankings?page=0&filters=QB|WR|RB|TE&format=2") %>% 
  html_node("#rankings-page-rankings")

.add_mergename <- function(Player) {
  Player %>%
    str_remove("( Jr.$)|( Sr.$)|( III$)|( II$)|( IV$)|( V$)|(\\')|(\\.)") %>%
    str_to_lower()
}

df_name <-
  tibble(Player = html_nodes(ktc,'a') %>% html_text(),
         Value = html_nodes(ktc,'.value p') %>% html_text()) %>% 
  mutate(Player = .add_mergename(Player))
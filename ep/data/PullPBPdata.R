library(furrr)
library(tidyverse)
library(here)
library(arrow)

plan(multisession)
#2000*1024^2
options(future.globals.maxSize= 1048576000)

memory.limit()
memory.limit(size=20000)

setwd(here())

fix_inconsistent_data_types <- function(p)
{
  p %>%
    mutate_at(vars(tidyselect::matches(".*player.*id.*|.*player.*name.*|.*team.*")), as.character) %>% 
    mutate_at(c("pass_length","pass_location","replay_or_challenge_result"), as.character)
}

read_csv_aschar <- function(url){ read_csv(url) %>% mutate_all(as.character) }

season_type <- c("regular_season/reg_pbp_","post_season/post_pbp_")
season_year <- seq(1999,2019)
games <- read_csv("http://www.habitatring.com/games.csv") #%>%
  #mutate(game_id = as.character(game_id))

df <- expand_grid(season_type, season_year) %>%
  mutate(user = case_when(season_year <= 2008 ~ "CroppedClamp",
                          TRUE ~ "ryurko"),
         link = paste0("https://raw.githubusercontent.com/",
                       user,
                       "/nflscrapR-data/master/play_by_play_data/",
                       season_type,
                       season_year,
                       ".csv"),
         #dataframe = future_map(link, read_csv_aschar, .progress = TRUE))
         dataframe = future_map(link, read_csv, .progress = TRUE))
         
df2 <- df %>%
  mutate(dataframe = future_map(dataframe, fix_inconsistent_data_types)) %>%
  unnest(cols=c(dataframe)) %>%
  left_join(games, by=c("game_id"))

write_parquet(df2, "data/pbp_data/pbp_reg_post_1999_2019.pdata")

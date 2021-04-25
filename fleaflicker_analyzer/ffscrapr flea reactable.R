library(ffscrapr)
library(tidyverse)
library(here)
library(reactable)
library(htmltools)

#remotes::install_github('dynastyprocess/ffscrapr', ref="flea/limiters", force=TRUE)
ffscrapr::ff_connect(playform="fleaflicker", league_id = "206154")
con <- ffscrapr::fleaflicker_connect(league_id = "206154", season = 2018)

franchises <- ffscrapr::ff_franchises(con) 
         
standings <- ffscrapr::ff_standings(con, season = 2018) %>% 
  #select(franchise_name, points_for, potential_points)
  
  select(-division_id, -division_name, -franchise_name) %>% 
  mutate(coach_rating = points_for/potential_points)

schedule <- ffscrapr::ff_schedule(con)

table_combo <- franchises %>% 
  inner_join(standings, by = c("franchise_id"))

table_combo %>%
  select(division_name, franchise_name, h2h_wins, points_for, allplay_winpct, coach_rating) %>% 
  reactable(
    groupBy = "division_name",
    columns = list(
      division_name = colDef(
        name = "Divison",
        minWidth = 40),
      franchise_name = colDef(
        name = "Franchise",
        cell = function(value) {
          div(
            img(alt = "", src = franchises %>% filter(franchise_name == value) %>% pull(franchise_logo), height = "40px"),
            value
          )
      })
      # top_12_seasons = colDef(name = "Top 12 Seasons",
      #                         aggregate = "sum",
      #                         minWidth = 20),
      # top_12_rate = colDef(name = "Top 12 Rate",
      #                      aggregate = JS("function(values, rows) {
      #                                         var totalSeason = 0
      #                                         var totalTop12 = 0
      #                                         rows.forEach(function(row) {
      #                                           totalSeason += row['total_seasons']
      #                                           totalTop12 += row['top_12_seasons']
      #                                         })
      #                                         return totalTop12 / totalSeason
      #                                       }"),
      #                      format = colFormat(percent = TRUE, digits = 1),
      #                      minWidth = 20),
    ),
    defaultPageSize = 20,
    bordered = TRUE
    #outlined = TRUE,
    #borderless = TRUE,
    #striped = TRUE
  )
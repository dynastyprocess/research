require(httr)
library(tidyverse)

headers = c(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:109.0) Gecko/20100101 Firefox/119.0",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
  `Accept-Language` = "en-US,en;q=0.5",
  `Accept-Encoding` = "gzip, deflate, br",
  `Referer` = "https://www.numberfire.com/nba/fantasy/weekly-projections/",
  `DNT` = "1",
  `Connection` = "keep-alive",
  `Upgrade-Insecure-Requests` = "1",
  `Sec-Fetch-Dest` = "document",
  `Sec-Fetch-Mode` = "navigate",
  `Sec-Fetch-Site` = "same-origin",
  `Sec-Fetch-User` = "?1",
  `TE` = "trailers"
)

scrape_numberfire_date <- function(date){
  
  Sys.sleep(10)
  
  params = list(
    `scope` = "total",
    `d` = date
  )
  
  res <- httr::GET(url = "https://www.numberfire.com/nba/fantasy/weekly-projections/",
                   httr::add_headers(.headers=headers),
                   query = params)
  
  numberfire <- 
    content(res) |> 
    rvest::html_table()
  
  projections <- 
    tibble(name = numberfire[[1]]$Player) |> 
    bind_cols(numberfire[[2]])
  
  return(projections)
  
}

scrape_years <- 
  tibble(date = as_date("2023-11-13")) |> 
  # mutate(date = as_date(date),
  #        day_of_week = wday(date, label = TRUE)) |> 
  # filter(day_of_week == "Mon") |> 
  mutate(numberfire_nested = map(.x = date,
                                 .f = scrape_numberfire_date,
                                 .progress = TRUE))

scrape_years_unnest <- 
  scrape_years |> 
  mutate(how_many_rows = map_dbl(numberfire_nested, nrow)) |> 
  filter(how_many_rows > 1) |> 
  unnest(numberfire_nested) |> 
  separate(col = name,
           into = c("name", "name_short", "pos_team"),
           sep = "\n") |> 
  mutate(across(c(name, name_short, pos_team),
                .f = str_trim),
         pos_team = str_remove_all(pos_team, "\\(|\\)")) |> 
  separate(col = pos_team,
           into = c("pos","team"),
           sep = ", ") |> 
  mutate(across(c(`FG%`, `FT%`, `3P%`),
                .f = ~as.numeric(str_remove(.x, "%"))/100)) |> 
  janitor::clean_names() |> 
  select(-how_many_rows)

week_ranges <- 
  tibble(date = as_date("2023-10-01"):as_date("2024-05-12")) |> 
  mutate(date = as_date(date),
         day_of_week = wday(date, label = TRUE)) |> 
  filter(day_of_week %in% c("Mon","Sun")) |> 
  mutate(week_end = lead(date)) |> 
  filter(day_of_week == "Mon")

numberfire_projections_range <- 
  scrape_years_unnest |> 
  mutate(date = as_date(date)) |> 
  left_join(week_ranges |> 
              select(date, week_end),
            by = c("date")) |> 
  transmute(week_start = date,
            week_end,
            name,
            pos,
            team,
            player_name = nflreadr::clean_player_names(name),
            player_name = map_chr(player_name, nba_name_mapper),
            proj_points = pts / g,
            proj_min = min / g,
            proj_fgm = fgm / g,
            proj_fga = fga / g,
            proj_fg_pct = fg_percent,
            proj_ftm = ftm / g,
            proj_fta = fta / g,
            proj_ft_pct = ft_percent,
            proj_3pm = x3pm / g,
            proj_3pa = x3pa / g,
            proj_3p_pct = x3p_percent,
            proj_reb = reb / g,
            proj_ast = ast / g,
            proj_stl = stl / g,
            proj_blk = blk / g,
            proj_tov = tov / g)

nba_player_box <- 
  hoopR::load_nba_player_box(2024:2024) |> 
  transmute(season,
            game_id,
            game_date,
            athlete_display_name,
            player_name = nflreadr::clean_player_names(athlete_display_name),
            team_display_name,
            team_abbreviation,
            opponent_team_display_name,
            opponent_team_abbreviation,
            minutes,
            field_goals_made,
            field_goals_attempted,             
            three_point_field_goals_made,      
            three_point_field_goals_attempted, 
            free_throws_made,                  
            free_throws_attempted,             
            offensive_rebounds,                
            defensive_rebounds,                
            rebounds,                         
            assists,                           
            steals,                            
            blocks,                        
            turnovers,                         
            fouls,
            points,
            starter,
            ejected,
            did_not_play,
            active,
            athlete_position_abbreviation,
            home_away,
            team_winner,
            team_score,
            opponent_team_score)

# TODO grab actual starter from Rotowire
new_game <- 
  nba_player_box |> 
  slice_max(game_date,
            by = c(season, player_name)) |> 
  distinct(season,
           player_name,
           team_abbreviation,
           athlete_position_abbreviation,
           starter
           ) |> 
  mutate(game_date = Sys.Date())


proj_box <- 
  nba_player_box |> 
  bind_rows(new_game) |> 
  left_join(numberfire_projections_range,
            by = join_by(player_name,
                         team_abbreviation == team,
                         between(game_date, week_start, week_end)))

home_teams <-
  hoopR::nba_schedule(season = 2023) |> 
  transmute(home_team_tricode, game_date, home_away = "home")

away_teams <-
  hoopR::nba_schedule(season = 2023) |> 
  transmute(away_team_tricode, game_date, home_away = "away")

prep_data <- 
  proj_box |> 
  select(-home_away) |> 
  left_join(home_teams,
            c("game_date",
              "team_abbreviation" = "home_team_tricode")) |> 
  left_join(away_teams,
            c("game_date",
              "team_abbreviation" = "away_team_tricode")) |> 
  mutate(home_away = coalesce(home_away.x, home_away.y)) |> 
  arrange(game_date) |> 
  mutate(across(.cols = c(minutes, assists, fouls, turnovers),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
                .names = "{.col}_roll8"),
         .by = c(season, player_name)) |> 
  filter(!is.na(proj_ast),
         !is.na(assists_roll8),
         game_date == Sys.Date(),
         !is.na(home_away)) |> 
  transmute(season,
            game_date,
            player_name,
            team_abbreviation,
            home_away,
            athlete_position_abbreviation,
            assists,
            assists_factor = case_when(assists < 11 ~ as.character(assists),
                                       assists >= 11 ~ "11+"),
            starter = as.factor(starter),
            proj_ast,
            proj_min,
            proj_tov,
            minutes_roll8,
            assists_roll8,
            fouls_roll8,
            turnovers_roll8)

library(tidymodels)
assist_predicts_wide <-
  prep_data %>%
  bind_cols(predict(
    final_fit,
    prep_data,
    type = "prob") %>% 
      rename_with(.fn = ~ paste0("assists", .x))
  )


library(odds.converter)
assist_preds_long <- 
  assist_predicts_wide %>% 
  select(player_name, game_date, athlete_position_abbreviation, contains("pred")) %>% 
  pivot_longer(cols = contains("pred")) %>% 
  mutate(assists_pred = as.numeric(str_remove(name, "assists.pred_")),
         assists_pred = replace_na(assists_pred, 11)) %>% 
  arrange(player_name, game_date, athlete_position_abbreviation, assists_pred) %>% 
  group_by(player_name, game_date, athlete_position_abbreviation) %>% 
  mutate(market_name = "assists",
         under_prob = cumsum(value),
         over_prob = 1 - under_prob,
         market_line =  assists_pred + 0.5,
         under_prob_us = round(odds.prob2us(under_prob)),
         over_prob_us = round(odds.prob2us(over_prob))) %>% 
  ungroup() %>% 
  rename(pred_category = assists_pred)


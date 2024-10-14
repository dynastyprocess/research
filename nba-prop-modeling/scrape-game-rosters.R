nba_player_box <- 
  hoopR::load_nba_player_box(2013:2024) |> 
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

get_rosters <- 
  nba_player_box |> 
  distinct(game_id) |> 
  mutate(game_rosters = map(.x = game_id,
                            .f = possibly(espn_nba_game_rosters),
                            .progress = TRUE))



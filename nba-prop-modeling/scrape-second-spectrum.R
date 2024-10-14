library(hoopR)
library(tidyverse)


possessions <- 
  crossing(season = 2016:2024) |> 
  mutate(season_string = year_to_season(season - 1),
         nested_stats = map(.x = season_string,
                            .f = ~nba_leaguedashplayerstats(per_mode = "Totals",
                                                            measure_type = "Advanced",
                                                            season = .x)))

possessions_unnest <- 
  possessions |> 
  unnest(nested_stats) |> 
  unnest(nested_stats) |> 
  transmute(season,
            PLAYER_ID,
            TEAM_ID,
            across(.cols = c(POSS),
                   .fns = as.numeric))

collect_all_the_stats <- 
  crossing(season = 2016:2024,
           measure_type = c("Drives",
                            "Defense",
                            "CatchShoot",
                            "Passing",
                            "Possessions",
                            "PullUpShot",
                            "Rebounding",
                            "Efficiency",
                            "SpeedDistance",
                            "ElbowTouch",
                            "PostTouch",
                            "PaintTouch")) |> 
  # slice_sample(n = 3) |> 
  mutate(season_string = year_to_season(season - 1),
         nested_data = map2(.x = measure_type,
                            .y = season_string,
                            .f = ~nba_leaguedashptstats(
                              per_mode = "Totals",
                              pt_measure_type = .x,
                              season = .y)
         ))

stats_unnest <- 
  collect_all_the_stats |> 
  unnest(nested_data) |> 
  unnest(nested_data) |> 
  mutate(across(.cols = c(everything(),
                          -measure_type,
                          -season_string,
                          -PLAYER_ID,
                          -PLAYER_NAME,
                          -TEAM_ID,
                          -TEAM_ABBREVIATION),
                .fns = as.numeric)) |> 
  select(season,
         PLAYER_NAME,
         PLAYER_ID,
         TEAM_ID,
         DRIVES,
         DIST_MILES_OFF,
         AVG_SPEED_OFF) |> 
  pivot_longer(cols = c(DRIVES,
                        DIST_MILES_OFF,
                        AVG_SPEED_OFF)) |> 
  filter(!is.na(value)) |> 
  distinct() |> 
  pivot_wider(id_cols = c(season,
                          TEAM_ID,
                          PLAYER_ID),
              names_from = name,
              values_from = value)

shooters <- 
  crossing(season = 2016:2024) |> 
  mutate(season_string = year_to_season(season - 1),
         nested_stats = map(.x = season_string,
                            .f = ~nba_leagueleaders(per_mode = "Totals",
                                                    season = .x)))

shooters_shoot <- 
  shooters |> 
  unnest(nested_stats) |> 
  unnest(nested_stats) |> 
  transmute(season,
            PLAYER_ID,
            PLAYER,
            TEAM_ID,
            TEAM,
            across(.cols = c(GP,
                             MIN,
                             FGA,
                             FG3A),
                   .fns = as.numeric),
            PCT_FGA_3FGA = FG3A / FGA)
  


collect_all_the_stats_2 <- 
  crossing(season = 2016:2024,
           measure_type = c('Isolation',
                            'Transition', 
                            'PRBallHandler',
                            'PRRollman',
                            'Postup',
                            'Spotup',
                            'Handoff',
                            'Cut',
                            'OffScreen',
                            'OffRebound',
                            'Misc')) |> 
  # slice_sample(n = 3) |>
  mutate(season_string = year_to_season(season - 1),
         nested_data = map2(.x = measure_type,
                            .y = season_string,
                            .f = ~nba_synergyplaytypes(
                              per_mode = "Totals",
                              play_type = .x,
                              season = .y)
         ))

stats_unnest2 <- 
  collect_all_the_stats_2 |> 
  unnest(nested_data) |> 
  unnest(nested_data) |> 
  mutate(across(.cols = c(everything(),
                          -measure_type,
                          -season_string,
                          -SEASON_ID,
                          -PLAYER_ID,
                          -PLAYER_NAME,
                          -TEAM_ID,
                          -TEAM_ABBREVIATION,
                          -TEAM_NAME,
                          -PLAY_TYPE,
                          -TYPE_GROUPING),
                .fns = as.numeric)) |> 
  # select(season,
  #        # measure_type,
  #        PLAYER_NAME,
  #        PLAYER_ID,
  #        # GP,
  #        # MIN,
  #        POSS_PCT) |> 
  # pivot_longer(cols = c()) |> 
  # filter(!is.na(value)) |> 
  # distinct() |> 
  pivot_wider(id_cols = c(season,
                          TEAM_ID,
                          PLAYER_ID),
              names_from = PLAY_TYPE,
              values_from = c(POSS, POSS_PCT)) |> 
  
  mutate(across(everything(),
                ~replace_na(.x, 0)),
         total_pct = POSS_PCT_Cut + POSS_PCT_Handoff + POSS_PCT_Isolation +
           POSS_PCT_Misc + POSS_PCT_OffRebound + POSS_PCT_OffScreen +
           POSS_PCT_Postup + POSS_PCT_PRBallHandler + POSS_PCT_PRRollMan + 
           POSS_PCT_Spotup + POSS_PCT_Transition,
         
         total_poss = POSS_Cut + POSS_Handoff + POSS_Isolation +
           POSS_Misc + POSS_OffRebound + POSS_OffScreen +
           POSS_Postup + POSS_PRBallHandler + POSS_PRRollMan + 
           POSS_Spotup + POSS_Transition,
         
         regular_poss_denom = 
           POSS_Cut + POSS_Handoff + POSS_Isolation +
           POSS_OffRebound + POSS_OffScreen +
           POSS_Postup + POSS_PRBallHandler + POSS_PRRollMan + 
           POSS_Spotup,
         
         POSS_PCT_Cut = POSS_Cut / regular_poss_denom,
         POSS_PCT_Handoff = POSS_Handoff / regular_poss_denom,
         POSS_PCT_Isolation = POSS_Isolation / regular_poss_denom,
         POSS_PCT_OffRebound = POSS_OffRebound / regular_poss_denom,
         POSS_PCT_OffScreen = POSS_OffScreen / regular_poss_denom,
         POSS_PCT_Postup = POSS_Postup / regular_poss_denom,
         POSS_PCT_PRBallHandler = POSS_PRBallHandler / regular_poss_denom,
         POSS_PCT_PRRollMan = POSS_PRRollMan / regular_poss_denom,
         POSS_PCT_Spotup = POSS_Spotup / regular_poss_denom,
         
         
         ballhandling_poss = POSS_PCT_Isolation + POSS_PCT_PRBallHandler,
         shooting_poss = POSS_PCT_Handoff + POSS_PCT_Spotup + POSS_PCT_OffScreen,
         post_poss = POSS_PCT_Cut + POSS_PCT_Postup + POSS_PCT_PRRollMan + POSS_PCT_OffRebound,
         
         synergy_role = case_when(ballhandling_poss > 0.50 &
                                    post_poss <= 0.20 &
                                    POSS_PCT_Isolation >= 0.15 ~ "domineering_ball_handler",
                                  ballhandling_poss > 0.50 &
                                    post_poss <= 0.20 &
                                    POSS_PCT_Isolation < 0.15 ~ "primary_ball_handler",
                                  ballhandling_poss > 0.33 &
                                    ballhandling_poss <= 0.5 &
                                    post_poss <= 0.20 ~ "second_ball_handler",
                                  ballhandling_poss > 0.33 &
                                    post_poss > 0.20 ~ "tall_ball_handler",
                                  ballhandling_poss <= 0.33 &
                                    POSS_PCT_Handoff + POSS_PCT_OffScreen > 0.20 ~
                                    "runaround_wing",
                                  ballhandling_poss <= 0.33 &
                                    POSS_PCT_Handoff + POSS_PCT_OffScreen <= 0.20 &
                                    POSS_PCT_Spotup > 0.3 &
                                    post_poss < 0.25 ~ "spot_up_wing",
                                  ballhandling_poss <= 0.33 &
                                    POSS_PCT_Handoff + POSS_PCT_OffScreen <= 0.20 &
                                    POSS_PCT_Spotup > 0.3 &
                                    post_poss >= 0.25 ~ "tall_spot_up_wing",
                                  ballhandling_poss <= 0.33 &
                                    POSS_PCT_Handoff + POSS_PCT_OffScreen <= 0.20 &
                                    POSS_PCT_Spotup <= 0.3 &
                                    POSS_PCT_PRRollMan <= 0.15 & 
                                    POSS_PCT_Postup <= 0.15 ~ "reluctant_shooter",
                                  POSS_PCT_Spotup > 0.15 &
                                    POSS_PCT_PRRollMan > 0.15 &
                                    POSS_PCT_Postup <= 0.15 &
                                    post_poss > 0.33 ~ "pick_pop_big",
                                  ballhandling_poss <= 0.33 &
                                    POSS_PCT_Postup > 0.15 & 
                                    post_poss > 0.33 ~ "versatile_big",
                                  POSS_PCT_Spotup < 0.15 &
                                    POSS_PCT_PRRollMan > 0.15 & 
                                    POSS_PCT_Postup <= 0.15 &
                                    post_poss > 0.50 ~ "roll_cut_big",
                                  
                                  TRUE ~ "scrub")) |> 
  relocate(synergy_role, .after = "PLAYER_ID")


combine_all_stats <- 
  possessions_unnest |> 
  left_join(shooters_shoot, by = c("season", "PLAYER_ID", "TEAM_ID")) |> 
  left_join(stats_unnest, by = c("season", "PLAYER_ID", "TEAM_ID")) |> 
  left_join(stats_unnest2, by = c("season", "PLAYER_ID", "TEAM_ID")) |> 
  mutate(drives_per_100_poss = (DRIVES / POSS) * 100,
         threes_per_100_poss = (FG3A / POSS) * 100,
         bball_index_role = case_when(
           POSS_PRBallHandler + POSS_PCT_Isolation >= 0.55 & POSS_PCT_Isolation <= 0.15 ~
             "primary_ball_handler",
           POSS_PRBallHandler >= 0.33 & POSS_PCT_Isolation <= 0.15 ~
             "secondary_ball_handler",
           POSS_PCT_Isolation >= 0.15 ~ "shot_creator",
           POSS_PCT_OffRebound >= 0.10 & POSS_PCT_Cut >= 0.15 ~ "athletic_finisher",
           drives_per_100_poss >= 11.5 ~ "slasher",
           POSS_PCT_OffScreen + POSS_PCT_Handoff >= 0.15 ~ "off_screen_shooter",
           threes_per_100_poss >= 8.5 & AVG_SPEED_OFF >= 4.75 ~ "movement_shooter",
           threes_per_100_poss >= 8.5 & AVG_SPEED_OFF < 4.75 ~ "stationary_shooter",
           
            
           
           TRUE ~ "scrub")) |> 
  relocate(bball_index_role, .after = "synergy_role")


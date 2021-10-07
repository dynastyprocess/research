
# SET-UP ------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(lubridate)
library(here)
library(janitor)
library(nflreadr)
library(ffscrapr)

setwd(here())

get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}
setwd("~/Documents/DynastyProcess/research/expected-points")
filenames <- list.files("./models", pattern="fit", full.names=TRUE)
obj_names <- str_remove_all(filenames,"./models/|.RDS")
models <- map(filenames, readRDS) %>% set_names(obj_names)

nflfastr_rosters <-
  nflfastR::fast_scraper_roster(2020) %>%
  dplyr::select(season, gsis_id, position, full_name, birth_date, sportradar_id) %>% 
  dplyr::mutate(position = dplyr::if_else(position %in% c("HB","FB"), "RB", position))


# RUSHING PREDICTIONS -----------------------------------------------------
rush_df <-
  nflreadr::load_pbp(2020) %>% 
  # arrow::open_dataset("~/Documents/DynastyProcess/db/data/nflfastr_pbp") %>% 
  # dplyr::filter(season >= start_year) %>% 
  # dplyr::collect() %>%
  # Restrict to rush plays
  dplyr::filter(play_type == "run", !str_detect(desc, "kneel|Aborted")) %>%
  dplyr::left_join(nflfastr_rosters, by = c("fantasy_player_id" = "gsis_id", "season"), na_matches = "never") %>%
  dplyr::filter(position %in% c("QB","RB","WR","TE")) %>%
  dplyr::mutate(game_month = month(game_date),
                game_month = if_else(game_month < 3, 12, game_month),
                game_week = week(game_date),
                game_week = if_else(game_week <= 30, 53, game_week),
                game_wday = as.character(wday(game_date, label = TRUE)),
                game_wday = case_when(game_wday %in% c("Tue","Wed","Fri","Sat") ~ "Other", TRUE ~ game_wday),
                game_time = hour(hms(start_time)),
                implied_total = case_when(posteam_type == "away" & spread_line<=0 ~ (total_line+spread_line)/2 - spread_line,
                                          posteam_type == "away" & spread_line>0 ~ (total_line-spread_line)/2,
                                          posteam_type == "home" & spread_line>0 ~ (total_line+spread_line)/2 - spread_line,
                                          posteam_type == "home" & spread_line<=0 ~ (total_line-spread_line)/2),
                rusher_age = get_age(birth_date, game_date, dec = TRUE),
                #Two Point Conversion fixes
                two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                                is.na(two_point_conv_result) & str_detect(desc, "ATTEMPT SUCCEEDS") ~ 1,
                                                TRUE ~ 0),
                score = if_else(rush_touchdown == 1 | two_point_converted == 1, 1, 0),
                rushing_yards = case_when(is.na(rushing_yards) & two_point_attempt == 1 & two_point_converted == 1 ~ yardline_100,
                                          is.na(rushing_yards) & two_point_attempt == 1 & two_point_converted == 0 ~ 0 ,
                                          TRUE ~ rushing_yards),
                down = if_else(two_point_attempt == 1, 4, down),
                surface = if_else(surface == "grass", "grass", "turf"),
                run_location = case_when(!is.na(run_location) ~ run_location,
                                         str_detect(desc, " left") ~ "left",
                                         str_detect(desc, " right") ~ "right",
                                         str_detect(desc, " middle") ~ "middle",
                                         TRUE ~ "unk"),
                run_gap = case_when(!is.na(run_gap) ~ run_gap,
                                    run_location == "middle" ~ "guard",
                                    str_detect(desc, " end") ~ "end",
                                    str_detect(desc, " tackle") ~ "tackle",
                                    str_detect(desc, " guard") ~ "guard",
                                    str_detect(desc, " middle") ~ "guard",
                                    TRUE ~ "unk"),
                temp = case_when(roof %in% c("closed", "dome") ~ 68L,
                                 is.na(temp) ~ 60L,
                                 TRUE ~ temp),
                wind = case_when(roof %in% c("closed", "dome") ~ 0L,
                                 is.na(wind) ~ 8L,
                                 TRUE ~ wind),
                rushing_fantasy_points = 6*rush_touchdown + 2*two_point_converted + 0.1*rushing_yards - 2*fumble_lost,
                season = factor(season, levels = as.character(c(2001:2020)), ordered = TRUE),
                week = factor(week, levels = as.character(c(1:21)), ordered = TRUE),
                game_month = factor(game_month, levels = as.character(c(9:12)), ordered = TRUE),
                game_week = factor(game_week, levels = as.character(c(36:53)), ordered = TRUE),
                game_time = factor(game_time, levels = as.character(c(9:23)), ordered = TRUE),
                qtr = factor(qtr, levels = as.character(c(1:6)), ordered = TRUE),
                down = factor(down, levels = as.character(c(1:4)), ordered = TRUE),
                goal_to_go = factor(goal_to_go, levels = as.character(c(0,1))),
                shotgun = factor(shotgun, levels = as.character(c(0,1))),
                no_huddle = factor(no_huddle, levels = as.character(c(0,1))),
                qb_dropback = factor(qb_dropback, levels = as.character(c(0,1))),
                qb_scramble = factor(qb_scramble, levels = as.character(c(0,1))),
                two_point_attempt = factor(two_point_attempt, levels = as.character(c(0,1))),
                score = factor(score, levels = as.character(c(0,1))),
                first_down = factor(first_down, levels = as.character(c(0,1))),
                run_gap_dir = paste(run_location, run_gap, sep = "_")) %>%
  dplyr::filter(run_gap_dir %in% c("left_end", "left_tackle", "left_guard", "middle_guard",
                                   "right_guard", "right_tackle", "right_end")) %>%
  dplyr::bind_cols(predict(models$fit_rush_yards, new_data = .)) %>% 
  dplyr::rename(rushing_yards_exp = .pred) %>%
  dplyr::bind_cols(predict(models$fit_rush_tds, new_data = ., type = "prob")) %>%
  dplyr::rename(rushing_td_exp = .pred_1) %>%
  dplyr::select(-.pred_0) %>% 
  dplyr::bind_cols(predict(models$fit_rush_fds, new_data = ., type = "prob")) %>%
  dplyr::rename(rushing_fd_exp = .pred_1) %>% 
  dplyr::transmute(season = substr(game_id, 1, 4),
                   week,
                   game_id,
                   play_id = as.factor(play_id),
                   play_description = desc,
                   player_id = fantasy_player_id,
                   full_name,
                   position,
                   posteam,
                   player_type = "rush",
                   attempt = 1, 
                   yards_gained = rushing_yards,
                   yards_gained_exp = rushing_yards_exp,
                   # score,
                   # rushing_score_exp = rushing_td_exp,
                   touchdown = rush_touchdown,
                   touchdown_exp = if_else(two_point_attempt == 1, 0, rushing_td_exp),
                   two_point_conv = two_point_converted,
                   two_point_conv_exp = if_else(two_point_attempt == 1, rushing_td_exp, 0),
                   first_down = as.numeric(first_down) - 1,
                   first_down_exp = rushing_fd_exp,
                   fantasy_points = rushing_fantasy_points,
                   fantasy_points_exp = 0.1*rushing_yards_exp + if_else(two_point_attempt == 1,
                                                                        2*rushing_td_exp,
                                                                        6*rushing_td_exp),
                   fumble_lost)


# RECEIVING PREDICTIONS ---------------------------------------------------
pass_df <-
  nflreadr::load_pbp(2020) %>% 
  # arrow::open_dataset("~/Documents/DynastyProcess/db/data/nflfastr_pbp") %>% 
  # dplyr::filter(season >= start_year) %>% 
  # dplyr::collect() %>%
  # Restrict to rush plays
  dplyr::filter(play_type == "pass", !str_detect(desc, "Aborted")) %>%
  dplyr::left_join(select(nflfastr_rosters,
                          gsis_id,
                          season,
                          passer_position = position,
                          passer_full_name = full_name,
                          passer_birth_date = birth_date),
                   by = c("passer_player_id" = "gsis_id", "season"),
                   na_matches = "never") %>%
  dplyr::left_join(select(nflfastr_rosters,
                          gsis_id,
                          season,
                          receiver_position = position,
                          receiver_full_name = full_name,
                          receiver_birth_date = birth_date),
                   by = c("receiver_player_id" = "gsis_id", "season"),
                   na_matches = "never") %>%
  dplyr::filter(passer_position %in% c("QB","RB","WR","TE")) %>% 
  dplyr::filter(receiver_position %in% c("QB","RB","WR","TE")) %>%
  dplyr::mutate(game_month = month(game_date),
                game_month = if_else(game_month < 3, 12, game_month),
                game_week = week(game_date),
                game_week = if_else(game_week <= 30, 53, game_week),
                game_wday = as.character(wday(game_date, label = TRUE)),
                game_wday = case_when(game_wday %in% c("Tue","Wed","Fri","Sat") ~ "Other",
                                      TRUE ~ game_wday),
                game_time = hour(hms(start_time)),
                implied_total = case_when(posteam_type == "away" & spread_line<=0 ~ (total_line+spread_line)/2 - spread_line,
                                          posteam_type == "away" & spread_line>0 ~ (total_line-spread_line)/2,
                                          posteam_type == "home" & spread_line>0 ~ (total_line+spread_line)/2 - spread_line,
                                          posteam_type == "home" & spread_line<=0 ~ (total_line-spread_line)/2),
                passer_age = get_age(passer_birth_date, game_date, dec = TRUE),
                receiver_age = get_age(receiver_birth_date, game_date, dec = TRUE),
                true_passer_pos = passer_position,
                passer_position = if_else(passer_position != "QB", "non-QB", passer_position),
                #Two Point Conversion fixes
                two_point_converted = case_when(two_point_conv_result == "success" ~ 1,
                                                is.na(two_point_conv_result) & str_detect(desc, "ATTEMPT SUCCEEDS") ~ 1,
                                                TRUE ~ 0),
                score = if_else(pass_touchdown == 1 | two_point_converted == 1, 1, 0),
                receiving_yards = case_when(is.na(receiving_yards) & two_point_attempt == 1 & two_point_converted == 1 ~ yardline_100,
                                            is.na(receiving_yards) & two_point_attempt == 1 & two_point_converted == 0 ~ 0,
                                            complete_pass == 0 ~ 0,
                                            TRUE ~ receiving_yards),
                air_yards = if_else(two_point_attempt == 1, yardline_100, air_yards),
                complete_pass = if_else(two_point_attempt == 1 & grepl("is complete", desc), 1, complete_pass),
                pass_complete = if_else(complete_pass == 1, "complete", "incomplete"),
                down = if_else(two_point_attempt == 1, 4, down),
                xpass = if_else(two_point_attempt == 1, 0.75, xpass),
                distance_to_sticks = air_yards - ydstogo,
                #Data Cleaning
                surface = if_else(surface == "grass", "grass", "turf"),
                pass_location = case_when(!is.na(pass_location) ~ pass_location,
                                          str_detect(desc, " left") ~ "left",
                                          str_detect(desc, " right") ~ "right",
                                          str_detect(desc, " middle") ~ "middle",
                                          TRUE ~ "unk"),
                temp = case_when(roof %in% c("closed", "dome") ~ 68L,
                                 is.na(temp) ~ 60L,
                                 TRUE ~ temp),
                wind = case_when(roof %in% c("closed", "dome") ~ 0L,
                                 is.na(wind) ~ 8L,
                                 TRUE ~ wind),
                receiving_fantasy_points = 6*pass_touchdown + 2*two_point_converted  + 0.1*receiving_yards - 2*fumble_lost + complete_pass,
                passing_fantasy_points =  4*pass_touchdown + 2*two_point_converted  + 0.04*receiving_yards - 2*fumble_lost - 2*interception,
                season = factor(season, levels = as.character(c(2001:2020)), ordered = TRUE),
                week = factor(week, levels = as.character(c(1:21)), ordered = TRUE),
                game_month = factor(game_month, levels = as.character(c(9:12)), ordered = TRUE),
                game_week = factor(game_week, levels = as.character(c(36:53)), ordered = TRUE),
                game_time = factor(game_time, levels = as.character(c(9:23)), ordered = TRUE),
                qtr = factor(qtr, levels = as.character(c(1:6)), ordered = TRUE),
                down = factor(down, levels = as.character(c(1:4)), ordered = TRUE),
                goal_to_go = factor(goal_to_go, levels = as.character(c(0,1))),
                shotgun = factor(shotgun, levels = as.character(c(0,1))),
                no_huddle = factor(no_huddle, levels = as.character(c(0,1))),
                qb_dropback = factor(qb_dropback, levels = as.character(c(0,1))),
                qb_scramble = factor(qb_scramble, levels = as.character(c(0,1))),
                two_point_attempt = factor(two_point_attempt, levels = as.character(c(0,1))),
                score = factor(score, levels = as.character(c(0,1))),
                first_down = factor(first_down, levels = as.character(c(0,1))),
                pass_complete = factor(pass_complete, levels = c("complete", "incomplete"), ordered = TRUE),
                interception = factor(interception, levels = as.character(c(0,1))),
                qb_hit = factor(qb_hit, levels = as.character(c(0,1)))) %>%
  dplyr::filter(!is.na(air_yards)) %>%
  dplyr::bind_cols(predict(models$fit_pass_completion, new_data = ., type = "prob")) %>% 
  dplyr::rename(pass_completion_exp = .pred_complete) %>%
  dplyr::bind_cols(predict(models$fit_pass_yards, new_data = .)) %>% 
  dplyr::rename(receiving_yards_exp = .pred) %>%
  dplyr::bind_cols(predict(models$fit_pass_td, new_data = ., type = "prob")) %>%
  dplyr::rename(rec_td_exp = .pred_1) %>%
  dplyr::select(-.pred_0) %>% 
  dplyr::bind_cols(predict(models$fit_pass_fd, new_data = ., type = "prob")) %>%
  dplyr::rename(rec_fd_exp = .pred_1) %>%
  dplyr::select(-.pred_0) %>%
  dplyr::bind_cols(predict(models$fit_pass_int, new_data = ., type = "prob")) %>%
  dplyr::rename(passing_int_exp = .pred_1) %>%
  dplyr::select(-.pred_0) %>%  
  
  dplyr::transmute(season = substr(game_id, 1, 4),
                   week,
                   game_id,
                   play_id = as.factor(play_id),
                   play_description = desc,
                   # fantasy_player_id,
                   # full_name,
                   # position,
                   posteam,
                   
                   pass.player_id = passer_player_id,
                   pass.full_name = passer_full_name,
                   pass.position = true_passer_pos,
                   
                   rec.player_id = receiver_player_id,
                   rec.full_name = receiver_full_name,
                   rec.position = receiver_position,
                   
                   posteam,
                   attempt = 1,
                   air_yards,
                   complete_pass,
                   complete_pass_exp = pass_completion_exp,
                   
                   yards_gained = receiving_yards,
                   yards_gained_exp = receiving_yards_exp,
                   
                   touchdown = pass_touchdown,
                   touchdown_exp = if_else(two_point_attempt == 1, 0, rec_td_exp),
                   two_point_conv = two_point_converted,
                   two_point_conv_exp = if_else(two_point_attempt == 1, rec_td_exp, 0),
                   first_down = as.numeric(first_down)  - 1,
                   first_down_exp = rec_fd_exp,
                   interception = as.numeric(interception) - 1,
                   interception_exp = passing_int_exp,
                   fumble_lost)


pass_df_pivot <- 
  pass_df %>% 
  pivot_longer(cols = c(pass.player_id, pass.full_name, pass.position, 
                        rec.player_id, rec.full_name, rec.position),
               names_to = c("player_type", ".value"),
               names_sep = "\\.") %>% 
  mutate(fantasy_points_exp = if_else(player_type == "rec",
                                      0.1*yards_gained_exp + complete_pass_exp + 6*touchdown_exp + 2*two_point_conv_exp,
                                      0.04*yards_gained_exp - 2*interception_exp + 4*touchdown_exp + 2*two_point_conv_exp),
         fantasy_points = if_else(player_type == "rec",
                                  6*touchdown + 2*two_point_conv  + 0.1*yards_gained - 2*fumble_lost + complete_pass,
                                  4*touchdown + 2*two_point_conv  + 0.04*yards_gained - 2*fumble_lost - 2*interception))

combined_df <- 
  pass_df_pivot %>% 
  bind_rows(rush_df) %>% 
  pivot_wider(id_cols = c(season, posteam, week, game_id, player_id, full_name, position),
              names_from = player_type,
              names_glue = "{player_type}_{.value}",
              values_fn = sum,
              values_from = where(is.numeric)
              ) %>%
  remove_empty(which = "cols") %>% 
  mutate(across(.cols = where(is.numeric),
                .fns =  ~replace_na(.x, 0)),
         across(.cols = where(is.numeric),
                .fns =  ~round(.x, 2))) %>% 
  rowwise() %>%
  mutate(total_yards_gained = sum(across(ends_with("yards_gained"), sum)),
         total_yards_gained_exp = sum(across(ends_with("yards_gained_exp"), sum)),
         total_touchdown = sum(across(ends_with("touchdown"), sum)),
         total_touchdown_exp = sum(across(ends_with("touchdown_exp"), sum)),
         total_first_down = sum(across(ends_with("first_down"), sum)),
         total_first_down_exp = sum(across(ends_with("first_down_exp"), sum)),
         total_fantasy_points = sum(across(ends_with("fantasy_points"), sum)),
         total_fantasy_points_exp = sum(across(ends_with("fantasy_points_exp"), sum)))

#Create diff fields
exp_fields <- combined_df %>% select(contains("exp")) %>% colnames() %>% str_remove_all("_exp")

for(f in exp_fields) {
  combined_df[paste0(f,"_diff")] <- combined_df[f]-combined_df[paste0(f,"_exp")]
}


#Pull in snap data
snaps <- load_snap_counts(2020) %>% 
  transmute(game_id, player, team, offense_snaps, offense_pct = offense_pct/100,
            pfr_id, season = as.character(season)) %>% 
  # left_join(select(load_rosters(), gsis_id, full_name, pfr_id, position), by = "pfr_id") %>% view()
  left_join(select(dp_playerids(), gsis_id, fantasypros_id, pfr_id, position), by = "pfr_id") %>%
  full_join(combined_df, by = c("game_id", "gsis_id" = "player_id", "season")) %>%
  filter(position.x %in% c("QB","RB","WR","TE") | position.y %in% c("QB","RB","WR","TE")) %>% 
  mutate(full_name = coalesce(full_name, player),
         team = coalesce(posteam, team),
         position = coalesce(position.y,position.x),
         season = coalesce(season, substr(game_id,1,4)),
         week = coalesce(as.numeric(week), as.numeric(substr(game_id,6,7))),
         across(where(is.numeric), ~replace_na(.x, 0))) %>%
  select(-c(player, pfr_id, position.x, position.y, posteam))
  
arrow::write_parquet(snaps, "expected_points_2020.pdata")

fp <-
  arrow::open_dataset("~/Documents/DynastyProcess/db/data/fp_ecr") %>%
  dplyr::collect() 

# fp %>% group_by(page_type, fp_page, ecr_type) %>% summarise(n(), min(scrape_date), max(scrape_date)) %>% view()

fp_ros <- fp %>% 
  mutate(page_class = case_when(str_detect(fp_page, "ros-qb") ~ "ros_qb",
                                str_detect(fp_page, "ros-ppr-rb") ~ "ros_rb",
                                str_detect(fp_page, "ros-ppr-wr") ~ "ros_wr",
                                str_detect(fp_page, "ros-ppr-te") ~ "ros_te",
                                TRUE ~ "skip")) %>% 
  filter(page_class != "skip",
         scrape_date %ni% c("2020-10-12","2020-10-16"),
         scrape_date >= "2020-09-10",
         scrape_date <= "2020-12-30") %>% 
  group_by(scrape_date) %>% 
  mutate(week = cur_group_id()) %>% 
  ungroup() %>% 
  filter(week > 1)

fp_preseason <- fp %>% 
  filter(str_detect(fp_page, "cheatsheets"),
         scrape_date == "2020-09-03",
         fp_page != "ppr-cheatsheets",
         pos %in% c("QB","RB","WR","TE")) %>% 
  transmute(id, preszn_ecr = ecr, pos)

fp_preseason %>% group_by(pos) %>% summarise(max_ecr = max(preszn_ecr)) %>% ungroup()
fp_ros %>% group_by(pos) %>% summarise(max_ecr = max(ecr)) %>% ungroup()

fp_combo <- 
  fp_ros %>% 
  left_join(select(fp_preseason, id, preszn_ecr), by = "id") %>% 
  transmute(id, ecr, pos, preszn_ecr, week, season = "2020") %>% 
  full_join(snaps, by = c("id" = "fantasypros_id", "season", "week")) %>% 
  mutate(position = coalesce(position, pos),
         preszn_ecr = case_when(is.na(preszn_ecr) & position == "QB" ~ 67,
                                is.na(preszn_ecr) & position == "RB" ~ 160,
                                is.na(preszn_ecr) & position == "WR" ~ 236,
                                is.na(preszn_ecr) & position == "TE" ~ 104,
                                TRUE ~ preszn_ecr),
         ecr = case_when(is.na(ecr) & position == "QB" ~ 82,
                         is.na(ecr) & position == "RB" ~ 142,
                         is.na(ecr) & position == "WR" ~ 230,
                         is.na(ecr) & position == "TE" ~ 114,
                         TRUE ~ ecr)) %>%
  filter(!is.na(offense_snaps)) %>% 
  select(-pos, -id) 

# week1_priors <- 
#   fp_combo %>% 
#   filter(season == "2019" | week == 1) %>% 
#   arrange(season, week) %>% 
#   group_by(gsis_id) %>% 
#   mutate(week = as.character(week),
#          across(.cols = where(is.numeric),
#                 .fns = ~slider::slide_dbl(.x, mean, .before = Inf, .after = -1))) %>% 
#   ungroup() %>% 
#   filter(season == "2020")

weekly2020 <- 
  fp_combo %>% 
  filter(season == "2020", week <= 16) %>% 
  group_by(gsis_id) %>%
  arrange(week) %>%
  mutate(# across(where(is.numeric), ~replace_na(.x, 0)),
    future_ppg = slider::slide_dbl(total_fantasy_points, ~mean(.x, na.rm = TRUE), .after = Inf),
    across(.cols = where(is.numeric) & !contains("ecr") & !week & !future_ppg,
           .fns = ~slider::slide_dbl(.x, ~mean(.x, na.rm = TRUE), .before = Inf, .after = -1))) %>%
  ungroup() %>% 
  na.omit() #drop players' first game of the szn


ecr_model <- function(df) {
  lm(future_ppg ~ ecr, data = df)
}

preszn_ecr_model <- function(df) {
  lm(future_ppg ~ preszn_ecr, data = df)
}

fp_model <- function(df) {
  lm(future_ppg ~ total_fantasy_points, data = df)
}

ep_model <- function(df) {
  lm(future_ppg ~ total_fantasy_points_exp, data = df)
}

combo_model <- function(df) {
  lm(future_ppg ~ preszn_ecr + ecr + total_fantasy_points + total_fantasy_points_exp, data = df)
}

models_ck <- 
  fp_combo %>% 
  select(position, season, week, total_fantasy_points,  total_fantasy_points_exp, full_name)

models <- 
  weekly2020 %>% 
  # filter(preszn_ecr <= 48) %>% 
  select(position, week, future_ppg, ecr, preszn_ecr, total_fantasy_points,  total_fantasy_points_exp, full_name) %>% 
  # na.omit() %>% 
  group_by(position, week) %>% 
  nest() %>% 
  mutate(ecr_model = map(data, ecr_model),
         ecr_glance = map(ecr_model, broom::glance),
         preszn_model = map(data, preszn_ecr_model),
         preszn_glance = map(preszn_model, broom::glance),
         fp_model = map(data, fp_model),
         fp_glance = map(fp_model, broom::glance),
         ep_model = map(data, ep_model),
         ep_glance = map(ep_model, broom::glance),
         combo_model = map(data, combo_model),
         combo_glance = map(combo_model, broom::glance)
         ) %>%
  ungroup() %>% 
  hoist(.col = ecr_glance, ecr_rsq = "r.squared") %>% 
  hoist(.col = preszn_glance, preszn_rsq = "r.squared") %>% 
  hoist(.col = fp_glance, fp_rsq = "r.squared") %>% 
  hoist(.col = ep_glance, ep_rsq = "r.squared") %>% 
  hoist(.col = combo_glance, combo_rsq = "r.squared")
  
models %>% 
  pivot_longer(cols = contains("rsq")) %>% 
  arrange(position, week) %>% 
  ggplot(aes(x = week, y = value, color = name)) +
  geom_path() +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1:16)) + 
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~position)


models %>% 
  pivot_longer(cols = contains("rsq")) %>% 
  arrange(position, week) %>% 
  ggplot(aes(x = week, y = value, color = name)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1:16)) + 
  theme(panel.grid.minor = element_blank()) +
  facet_wrap(~position)

## Try a catboost because why not

training_resamples <-
  weekly2020 %>%
  vfold_cv(v = 5)

#Create recipe
ppg_recipe <- 
  recipe(future_ppg ~ ., data = weekly2020) %>%
  update_role(c(season, game_id, team, gsis_id, full_name, position), new_role = "id")

# prepped <- prep(ppg_recipe)
# juiced <- juice(prepped)

all_cores <- parallelly::availableCores() - 1
future::plan("multisession", workers = all_cores)


# Boosted Decision Tree
ppg_boost <- 
  boost_tree(mode = "regression",
             engine = "catboost",
             mtry = tune(),  
             trees = 1000,
             min_n = tune(),
             tree_depth = tune(),
             learn_rate = tune(),
             sample_size = 1)

# Workflow Sets
ppg_wf <- workflow(ppg_recipe, ppg_boost)

new_params <- 
  parameters(ppg_wf) %>%
  update(mtry = mtry(range = c(1,4)),
         min_n = min_n(range = c(100,1000)),
         tree_depth = tree_depth(range = c(2,3)),
         learn_rate = learn_rate(range = c(-2.5, -0.5), trans = scales::log10_trans()))

# Bayesian Tuning
ctrl_bayes <-
  control_bayes(
    verbose = TRUE,
    no_improve = 10,
    uncertain = 5,
    seed = 815,
    save_pred = TRUE,
    parallel_over = 'everything',
    save_workflow = TRUE
  )

res_grid_save <- res_grid

res_grid <-
  tune_bayes(
    ppg_wf,
    resamples = training_resamples,
    iter = 100,
    param_info = new_params,
    metrics = metric_set(rmse),
    initial = 10,
    control = ctrl_bayes
  )

res_grid %>% collect_metrics()

best_model <- select_best(res_grid, metric = "rmse")

# best_model <-  tibble(mtry=47, trees=1938, min_n=855, tree_depth=10, learn_rate=0.0647)

finalize_ppg <- finalize_workflow(ppg_wf, best_model)

fit_ppg <- fit(finalize_ppg, weekly2020)

library(DALEXtra)
ppg_explainer <-
  explain_tidymodels(
    fit_ppg,
    data = select(weekly2020, -c(future_ppg)),
    y =  weekly2020$future_ppg)

# plot(feature_importance(ppg_explainer))
vip_df <-  model_parts(ppg_explainer)

vip_df %>% 
  # filter(variable %in% c("_baseline_", "_full_model_") | mean_dropout_loss >= 1) %>% 
  plot()

pdp_time <- 
  model_profile(
    ppg_explainer,
    variables = "preszn_ecr",
    groups = "position"
  )

plot(pdp_time)

test_obs <- weekly2020 %>% filter(full_name == "Tee Higgins", week == 2)
test_obs$future_ppg

predict_parts(explainer = ppg_explainer, new_observation = test_obs) %>% plot()

weekly2020_fit <-
  weekly2020 %>%
  bind_cols(predict(fit_ppg, weekly2020)) %>%
  rename(future_ppg_pred = .pred) %>% 
  select(full_name, week, position, ecr, preszn_ecr, future_ppg_pred, future_ppg)

rmse_vec(weekly2020_fit$future_ppg_pred, weekly2020_fit$future_ppg)


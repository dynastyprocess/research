
# LOAD LIBRARIES ----------------------------------------------------------

library(ffscrapr)
library(ffpros)
library(tidyverse)
library(gt)
library(RColorBrewer)

league_conn <- ff_connect(platform = "sleeper", league_id = "652718526494253056", season = 2021)

league_settings <- ff_league(league_conn)

get_projections <- function(pos){fp_projections(page = pos, sport = "nfl", year = 2021, week = 1)}
         
projections_df <- 
  tibble(position = c("qb","rb","wr","te")) %>% 
  mutate(projections = map(position, get_projections),
         position = str_to_upper(position)) %>% 
  unnest(projections) %>% 
  pivot_longer(cols = where(is.numeric))

stat_mapping <-
  ffscrapr::nflfastr_stat_mapping %>%
  mutate(stat_name = case_when(nflfastr_event == "passing_yards" ~ "passing_yds",
                               nflfastr_event == "completions" ~ "passing_cmp",
                               nflfastr_event == "attempts" ~ "passing_att",
                               nflfastr_event == "interceptions" ~ "passing_ints",
                               nflfastr_event == "carries" ~ "rushing_att",
                               nflfastr_event == "rushing_yards" ~ "rushing_yds",
                               str_detect(nflfastr_event, "fumbles_lost")  ~ "misc_fl",
                               nflfastr_event == "receptions" ~ "receiving_rec",
                               nflfastr_event == "receiving_yards" ~ "receiving_yds",
                               TRUE ~ nflfastr_event)) %>% 
         filter(platform == "sleeper") %>% 
  select(-nflfastr_event) %>% 
  distinct()
         
league_rules <- ff_scoring(league_conn)

join_rules <- 
  projections_df %>% 
  inner_join(stat_mapping, by = c("name"="stat_name"), na_matches ="never") %>% 
  left_join(league_rules, by = c("ff_event"="event","position"="pos"), na_matches ="never") %>% 
  mutate(projected_points = value*points) %>% 
  group_by(position, fantasypros_id, player_name, team) %>% 
  summarise(projected_points = sum(projected_points, na.rm = TRUE))

league_rosters <- ff_rosters(league_conn)

rankings_df <- 
  if(league_settings$qb_type == "2QB/SF" & str_detect(league_settings$scoring_flags,"1_ppr")) {
    fp_rankings(page = "ppr-superflex", sport = "nfl")
  } else if (league_settings$qb_type == "2QB/SF") {
    fp_rankings(page = "superflex", sport = "nfl")
  } else if (str_detect(league_settings$scoring_flags,"1_ppr")) {
    fp_rankings(page = "ppr-flex", sport = "nfl") %>% 
      bind_rows(fp_rankings(page = "qb", sport = "nfl"))
  } else {fp_rankings(page = "flex", sport = "nfl") %>% 
      bind_rows(fp_rankings(page = "qb", sport = "nfl"))}

rosters <-
  rankings_df %>% 
  left_join(select(ffscrapr::dp_playerids(), fantasypros_id, sleeper_id), by = "fantasypros_id") %>%
  left_join(join_rules, by = "fantasypros_id") %>% 
  left_join(league_rosters, by = c("sleeper_id"="player_id")) %>% 
  transmute(franchise_name,
            player_name,
            position = factor(position, levels = c("QB","RB","WR","TE"), ordered = TRUE),
            team,
            rank,
            ecr,
            best,
            worst,
            projected_points)

# league_starters <- ff_starter_positions(league_conn)

rosters %>% 
  filter(franchise_name == "Galletti") %>% 
  arrange(position, -projected_points, -ecr) %>% 
  gt() %>%
  tab_header(title = "Start/Sit Guide") %>%
  cols_label(franchise_name = "Franchise Name",
             player_name = "Player",
             position = "Position",
             team = "Team",
             rank = "Position",
             ecr = "Consensus",
             best = "Best",
             worst = "Worst",
             projected_points = "Projected Points") %>% 
  tab_spanner(label = "Rank",
              columns = c(rank, ecr, best, worst)) %>% 
  data_color(
    columns = c(projected_points),
    colors = scales::col_factor(
      brewer.pal(11,'PRGn')[3:8],
      domain = NULL
    ))%>% 
  data_color(
    columns = c(ecr),
    colors = scales::col_factor(
      brewer.pal(11,'PRGn')[8:3],
      domain = NULL
    ))
  
  
  

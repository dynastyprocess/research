suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(slider)
  library(lubridate)
  library(tidymodels)
  library(mgcv)

})


# Pull in DB data ---------------------------------------------------------
setwd(here::here())
memory.size(60000)

con <- DBI::dbConnect(odbc::odbc(), "dynastyprocess")

ep <- read_parquet("C:\\Users\\syd23\\Documents\\DynastyProcess\\apps\\expectedpoints\\ep_1999_2019.pdata") 
  #dbGetQuery(con, "SELECT * FROM dp_expectedpoints WHERE Pos in ('QB','RB','WR','TE')")

ecr_archive_pos <- dbGetQuery(con, "SELECT week, year, sportsdata_id, page_pos as pos, ecr as ecr_ovr from fp_ecr_archive
                                where page_type in ('weekly-qb','weekly-rb','weekly-wr','weekly-te') and sportsdata_id is not null") %>% 
  mutate(across(.cols = c("week","year"),
                .fns = as.factor))

ecr_archive_ovr <- dbGetQuery(con, "SELECT week, year, sportsdata_id, pos, ecr as ecr_pos from fp_ecr_archive
                                where page_type = 'weekly-op' and sportsdata_id is not null") %>% 
  mutate(across(.cols = c("week","year"),
                .fns = as.factor))
  
pbp <- dbGetQuery(con, "SELECT distinct game_id, posteam, posteam_type, total_line,
                               case when posteam_type = 'away' then spread_line else -spread_line end as 'spread_line'
                        FROM nflfastr_pbp where posteam is not null and posteam <> ''") %>% 
  mutate(posteam = case_when(posteam == "ARZ" ~ "ARI",
                             posteam == "BLT" ~ "BAL",
                             posteam == "CLV" ~ "CLE",
                             posteam == "HST" ~ "HOU",
                             posteam == "JAC" ~ "JAX",
                             posteam == "LA" ~ "LAR",
                             posteam == "STL" ~ "LAR",
                             posteam == "SAN" ~ "LAC",
                             posteam == "SD" ~ "LAC",
                             posteam == "SL" ~ "LAR",
                             TRUE ~ posteam))

dbDisconnect(con)
rm(con)

# dup_check <- ep %>% 
#   group_by(gsis_id, Name) %>%
#   summarise(pos_count = n_distinct(Pos), sum(total_fp)) %>% 
#   filter(pos_count > 1)

# Functions -------------------------------------------------------------
get_rate <- function(x,y){
  rate <- sum(x, na.rm = TRUE) / sum(y, na.rm = TRUE)
  
  ifelse(is.nan(rate) | is.infinite(rate), 0, rate)
}

clean_pre2006 <- function(year, value) {
  ifelse(year < 2006, NA, value)
}

features_wide <- ep %>%
  select(-week_season_num) %>%
  #filter(Name == "Peyton Manning") %>% 
  #filter(Name == "Christian Kirk") %>%  #| Name == "Sam Darnold") %>%
  inner_join(pbp, by = c("gsis_game_id" = "game_id", "Team" = "posteam")) %>%
  mutate(across(.cols = c("Week"),
                .fns = as.factor)) %>% 
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id, Season) %>%
  mutate(parlay_td = if_else(rush_td + rec_td > 0,1,0),
         implied_total = if_else(spread_line<=0, (total_line+spread_line)/2 - spread_line, (total_line-spread_line)/2),
         across(.cols = c(pass_fp_x, pass_fp_team_x, pass_fp_diff, pass_fp_team_diff,
                          pass_ay, pass_ay_team, pass_yac,
                          pass_comp_x, pass_comp_diff, pass_comp_team,
                          pass_yd_x, pass_yd_diff, pass_yd_team_x, pass_yd_team_diff,
                          pass_td_x, pass_td_diff, pass_td_team_x, pass_td_team_diff,
                          
                          rec_fp_x, rec_fp_team_x, rec_fp_diff, rec_fp_team_diff,
                          rec_ay, rec_yac,
                          rec_comp_x, rec_comp_diff,
                          rec_yd_x, rec_yd_diff, rec_yd_team_x, rec_yd_team_diff,
                          rec_td_x, rec_td_diff, rec_td_team_x, rec_td_team_diff,
                          
                          total_fp_x, total_fp_team_x, total_fp_diff, total_fp_team_diff,
                          total_yd_x, total_yd_diff, total_yd_team_x, total_yd_team_diff,
                          total_td_x, total_td_diff, total_td_team_x, total_td_team_diff),
                .fns = ~clean_pre2006(Season, .x)),
         across(.cols = where(is.numeric),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = Inf),
                .names = "{.col}_season"),
         rec_racr_season = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = Inf),
         rec_tar_share_season = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = Inf),
         rec_ay_share_season = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = Inf),
         rec_wopr_season = 1.5*clean_pre2006(Season,rec_tar_share_season) + 0.7*rec_ay_share_season,
         rec_ypt_season = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = Inf),
         rec_comp_rate_season = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = Inf),
         rec_td_rate_season = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = Inf),
         rec_yptpa_season = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = Inf),
         rec_adot_season = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = Inf),
         rec_yac_rate_season = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = Inf),
         rush_ypc_season = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = Inf),
         rush_td_rate_season = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = Inf),
         rush_fp_share_season = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = Inf),
         rec_fp_share_season = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = Inf),
         total_fp_share_season = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = Inf),
         rush_fp_x_share_season = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = Inf),
         rec_fp_x_share_season = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = Inf),
         total_fp_x_share_season = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y),
                                                            .before = Inf),
         pass_pacr_season = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = Inf),
         pass_adot_season = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = Inf),
         pass_comp_rate_season = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = Inf),
         pass_td_rate_season = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = Inf),
         pass_yac_rate_season = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = Inf)) %>% 
  ungroup() %>% 
  group_by(gsis_id) %>%
  mutate(across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 1),
                .names = "{.col}_roll2"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 2),
                .names = "{.col}_roll3"),         
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 3),
                .names = "{.col}_roll4"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 4),
                .names = "{.col}_roll5"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 5),
                .names = "{.col}_roll6"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 6),
                .names = "{.col}_roll7"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 7),
                .names = "{.col}_roll8"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 8),
                .names = "{.col}_roll9"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 9),
                .names = "{.col}_roll10"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 10),
                .names = "{.col}_roll11"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 11),
                .names = "{.col}_roll12"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 12),
                .names = "{.col}_roll13"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 13),
                .names = "{.col}_roll14"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 14),
                .names = "{.col}_roll15"),  
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = 15),
                .names = "{.col}_roll16"),
         across(.cols = where(is.numeric) & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~sum(.x, na.rm =TRUE), .before = Inf),
                .names = "{.col}_career"),

         rec_racr = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y)),
         rec_tar_share = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y)),
         rec_ay_share = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y)),
         rec_wopr = 1.5*clean_pre2006(Season,rec_tar_share) + 0.7*rec_ay_share,
         rec_ypt = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y)),
         rec_comp_rate = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y)),
         rec_td_rate = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y)),
         rec_yptpa = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y)),
         rec_yac_rate = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y)),
         rush_ypc = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y)),
         rush_td_rate = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y)),
         rush_fp_share = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y)),
         rec_fp_share = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y)),
         total_fp_share = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y)),
         rush_fp_x_share = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y)),
         rec_fp_x_share = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y)),
         total_fp_x_share = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y),
                                             .before = 1),
         pass_pacr = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y)),
         pass_comp_rate = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y)),
         pass_td_rate = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y)),
         pass_yac_rate = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y)),
         pass_adot = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y)),
         rec_adot= slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y)),
         
         rec_racr_roll2 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 1),
         rec_tar_share_roll2 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 1),
         rec_ay_share_roll2 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 1),
         rec_wopr_roll2 = 1.5*clean_pre2006(Season,rec_tar_share_roll2) + 0.7*rec_ay_share_roll2,
         rec_ypt_roll2 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 1),
         rec_comp_rate_roll2 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 1),
         rec_td_rate_roll2 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 1),
         rec_yptpa_roll2 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 1),
         rec_yac_rate_roll2 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 1),
         rush_ypc_roll2 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 1),
         rush_td_rate_roll2 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 1),
         rush_fp_share_roll2 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 1),
         rec_fp_share_roll2 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 1),
         total_fp_share_roll2 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 1),
         rush_fp_x_share_roll2 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 1),
         rec_fp_x_share_roll2 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 1),
         total_fp_x_share_roll2 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 1),
         pass_pacr_roll2 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 1),
         pass_comp_rate_roll2 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 1),
         pass_td_rate_roll2 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 1),
         pass_yac_rate_roll2 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 1),
         pass_adot_roll2 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 1),
         rec_adot_roll2 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 1),
         
         rec_racr_roll3 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 2),
         rec_tar_share_roll3 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 2),
         rec_ay_share_roll3 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 2),
         rec_wopr_roll3 = 1.5*clean_pre2006(Season,rec_tar_share_roll3) + 0.7*rec_ay_share_roll3,
         rec_ypt_roll3 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 2),
         rec_comp_rate_roll3 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 2),
         rec_td_rate_roll3 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 2),
         rec_yptpa_roll3 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 2),
         rec_yac_rate_roll3 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 2),
         rush_ypc_roll3 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 2),
         rush_td_rate_roll3 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 2),
         rush_fp_share_roll3 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 2),
         rec_fp_share_roll3 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 2),
         total_fp_share_roll3 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 2),
         rush_fp_x_share_roll3 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 2),
         rec_fp_x_share_roll3 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 2),
         total_fp_x_share_roll3 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 2),
         pass_pacr_roll3 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 2),
         pass_comp_rate_roll3 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 2),
         pass_td_rate_roll3 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 2),
         pass_yac_rate_roll3 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 2),
         pass_adot_roll3 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 2),
         rec_adot_roll3 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 2),
         
         rec_racr_roll4 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 3),
         rec_tar_share_roll4 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 3),
         rec_ay_share_roll4 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 3),
         rec_wopr_roll4 = 1.5*clean_pre2006(Season,rec_tar_share_roll4) + 0.7*rec_ay_share_roll4,
         rec_ypt_roll4 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 3),
         rec_comp_rate_roll4 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 3),
         rec_td_rate_roll4 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 3),
         rec_yptpa_roll4 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 3),
         rec_yac_rate_roll4 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 3),
         rush_ypc_roll4 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 3),
         rush_td_rate_roll4 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 3),
         rush_fp_share_roll4 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 3),
         rec_fp_share_roll4 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 3),
         total_fp_share_roll4 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 3),
         rush_fp_x_share_roll4 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 3),
         rec_fp_x_share_roll4 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 3),
         total_fp_x_share_roll4 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 3),
         pass_pacr_roll4 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 3),
         pass_comp_rate_roll4 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 3),
         pass_td_rate_roll4 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 3),
         pass_yac_rate_roll4 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 3),
         pass_adot_roll4 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 3),
         rec_adot_roll4 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 3),
         
         rec_racr_roll5 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 4),
         rec_tar_share_roll5 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 4),
         rec_ay_share_roll5 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 4),
         rec_wopr_roll5 = 1.5*clean_pre2006(Season,rec_tar_share_roll5) + 0.7*rec_ay_share_roll5,
         rec_ypt_roll5 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 4),
         rec_comp_rate_roll5 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 4),
         rec_td_rate_roll5 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 4),
         rec_yptpa_roll5 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 4),
         rec_yac_rate_roll5 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 4),
         rush_ypc_roll5 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 4),
         rush_td_rate_roll5 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 4),
         rush_fp_share_roll5 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 4),
         rec_fp_share_roll5 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 4),
         total_fp_share_roll5 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 4),
         rush_fp_x_share_roll5 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 4),
         rec_fp_x_share_roll5 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 4),
         total_fp_x_share_roll5 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 4),
         pass_pacr_roll5 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 4),
         pass_comp_rate_roll5 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 4),
         pass_td_rate_roll5 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 4),
         pass_yac_rate_roll5 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 4),
         pass_adot_roll5 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 4),
         rec_adot_roll5 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 4),
         
         rec_racr_roll6 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 5),
         rec_tar_share_roll6 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 5),
         rec_ay_share_roll6 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 5),
         rec_wopr_roll6 = 1.5*clean_pre2006(Season,rec_tar_share_roll6) + 0.7*rec_ay_share_roll6,
         rec_ypt_roll6 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 5),
         rec_comp_rate_roll6 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 5),
         rec_td_rate_roll6 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 5),
         rec_yptpa_roll6 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 5),
         rec_yac_rate_roll6 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 5),
         rush_ypc_roll6 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 5),
         rush_td_rate_roll6 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 5),
         rush_fp_share_roll6 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 5),
         rec_fp_share_roll6 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 5),
         total_fp_share_roll6 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 5),
         rush_fp_x_share_roll6 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 5),
         rec_fp_x_share_roll6 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 5),
         total_fp_x_share_roll6 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 5),
         pass_pacr_roll6 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 5),
         pass_comp_rate_roll6 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 5),
         pass_td_rate_roll6 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 5),
         pass_yac_rate_roll6 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 5),
         pass_adot_roll6 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 5),
         rec_adot_roll6 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 5),
         
         rec_racr_roll7 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 6),
         rec_tar_share_roll7 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 6),
         rec_ay_share_roll7 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 6),
         rec_wopr_roll7 = 1.5*clean_pre2006(Season,rec_tar_share_roll7) + 0.7*rec_ay_share_roll7,
         rec_ypt_roll7 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 6),
         rec_comp_rate_roll7 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 6),
         rec_td_rate_roll7 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 6),
         rec_yptpa_roll7 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 6),
         rec_yac_rate_roll7 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 6),
         rush_ypc_roll7 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 6),
         rush_td_rate_roll7 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 6),
         rush_fp_share_roll7 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 6),
         rec_fp_share_roll7 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 6),
         total_fp_share_roll7 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 6),
         rush_fp_x_share_roll7 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 6),
         rec_fp_x_share_roll7 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 6),
         total_fp_x_share_roll7 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 6),
         pass_pacr_roll7 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 6),
         pass_comp_rate_roll7 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 6),
         pass_td_rate_roll7 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 6),
         pass_yac_rate_roll7 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 6),
         pass_adot_roll7 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 6),
         rec_adot_roll7 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 6),
         
         rec_racr_roll8 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 7),
         rec_tar_share_roll8 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 7),
         rec_ay_share_roll8 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 7),
         rec_wopr_roll8 = 1.5*clean_pre2006(Season,rec_tar_share_roll8) + 0.7*rec_ay_share_roll8,
         rec_ypt_roll8 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 7),
         rec_comp_rate_roll8 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 7),
         rec_td_rate_roll8 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 7),
         rec_yptpa_roll8 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 7),
         rec_yac_rate_roll8 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 7),
         rush_ypc_roll8 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 7),
         rush_td_rate_roll8 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 7),
         rush_fp_share_roll8 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 7),
         rec_fp_share_roll8 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 7),
         total_fp_share_roll8 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 7),
         rush_fp_x_share_roll8 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 7),
         rec_fp_x_share_roll8 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 7),
         total_fp_x_share_roll8 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 7),
         pass_pacr_roll8 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 7),
         pass_comp_rate_roll8 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 7),
         pass_td_rate_roll8 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 7),
         pass_yac_rate_roll8 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 7),
         pass_adot_roll8 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 7),
         rec_adot_roll8 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 7),
         
         rec_racr_roll9 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 8),
         rec_tar_share_roll9 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 8),
         rec_ay_share_roll9 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 8),
         rec_wopr_roll9 = 1.5*clean_pre2006(Season,rec_tar_share_roll9) + 0.7*rec_ay_share_roll9,
         rec_ypt_roll9 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 8),
         rec_comp_rate_roll9 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 8),
         rec_td_rate_roll9 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 8),
         rec_yptpa_roll9 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 8),
         rec_yac_rate_roll9 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 8),
         rush_ypc_roll9 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 8),
         rush_td_rate_roll9 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 8),
         rush_fp_share_roll9 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 8),
         rec_fp_share_roll9 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 8),
         total_fp_share_roll9 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 8),
         rush_fp_x_share_roll9 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 8),
         rec_fp_x_share_roll9 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 8),
         total_fp_x_share_roll9 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 8),
         pass_pacr_roll9 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 8),
         pass_comp_rate_roll9 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 8),
         pass_td_rate_roll9 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 8),
         pass_yac_rate_roll9 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 8),
         pass_adot_roll9 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 8),
         rec_adot_roll9 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 8),
         
         rec_racr_roll10 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 9),
         rec_tar_share_roll10 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 9),
         rec_ay_share_roll10 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 9),
         rec_wopr_roll10 = 1.5*clean_pre2006(Season,rec_tar_share_roll10) + 0.7*rec_ay_share_roll10,
         rec_ypt_roll10 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 9),
         rec_comp_rate_roll10 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 9),
         rec_td_rate_roll10 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 9),
         rec_yptpa_roll10 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 9),
         rec_yac_rate_roll10 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 9),
         rush_ypc_roll10 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 9),
         rush_td_rate_roll10 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 9),
         rush_fp_share_roll10 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 9),
         rec_fp_share_roll10 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 9),
         total_fp_share_roll10 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 9),
         rush_fp_x_share_roll10 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 9),
         rec_fp_x_share_roll10 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 9),
         total_fp_x_share_roll10 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 9),
         pass_pacr_roll10 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 9),
         pass_comp_rate_roll10 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 9),
         pass_td_rate_roll10 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 9),
         pass_yac_rate_roll10 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 9),
         pass_adot_roll10 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 9),
         rec_adot_roll10 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 9),
         
         rec_racr_roll11 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 10),
         rec_tar_share_roll11 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 10),
         rec_ay_share_roll11 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 10),
         rec_wopr_roll11 = 1.5*clean_pre2006(Season,rec_tar_share_roll11) + 0.7*rec_ay_share_roll11,
         rec_ypt_roll11 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 10),
         rec_comp_rate_roll11 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 10),
         rec_td_rate_roll11 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 10),
         rec_yptpa_roll11 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 10),
         rec_yac_rate_roll11 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 10),
         rush_ypc_roll11 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 10),
         rush_td_rate_roll11 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 10),
         rush_fp_share_roll11 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 10),
         rec_fp_share_roll11 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 10),
         total_fp_share_roll11 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 10),
         rush_fp_x_share_roll11 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 10),
         rec_fp_x_share_roll11 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 10),
         total_fp_x_share_roll11 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 10),
         pass_pacr_roll11 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 10),
         pass_comp_rate_roll11 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 10),
         pass_td_rate_roll11 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 10),
         pass_yac_rate_roll11 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 10),
         pass_adot_roll11 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 10),
         rec_adot_roll11 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 10),
         
         rec_racr_roll12 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 11),
         rec_tar_share_roll12 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 11),
         rec_ay_share_roll12 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 11),
         rec_wopr_roll12 = 1.5*clean_pre2006(Season,rec_tar_share_roll12) + 0.7*rec_ay_share_roll12,
         rec_ypt_roll12 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 11),
         rec_comp_rate_roll12 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 11),
         rec_td_rate_roll12 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 11),
         rec_yptpa_roll12 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 11),
         rec_yac_rate_roll12 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 11),
         rush_ypc_roll12 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 11),
         rush_td_rate_roll12 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 11),
         rush_fp_share_roll12 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 11),
         rec_fp_share_roll12 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 11),
         total_fp_share_roll12 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 11),
         rush_fp_x_share_roll12 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 11),
         rec_fp_x_share_roll12 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 11),
         total_fp_x_share_roll12 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 11),
         pass_pacr_roll12 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 11),
         pass_comp_rate_roll12 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 11),
         pass_td_rate_roll12 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 11),
         pass_yac_rate_roll12 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 11),
         pass_adot_roll12 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 11),
         rec_adot_roll12 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 11),
         
         rec_racr_roll13 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 12),
         rec_tar_share_roll13 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 12),
         rec_ay_share_roll13 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 12),
         rec_wopr_roll13 = 1.5*clean_pre2006(Season,rec_tar_share_roll13) + 0.7*rec_ay_share_roll13,
         rec_ypt_roll13 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 12),
         rec_comp_rate_roll13 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 12),
         rec_td_rate_roll13 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 12),
         rec_yptpa_roll13 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 12),
         rec_yac_rate_roll13 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 12),
         rush_ypc_roll13 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 12),
         rush_td_rate_roll13 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 12),
         rush_fp_share_roll13 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 12),
         rec_fp_share_roll13 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 12),
         total_fp_share_roll13 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 12),
         rush_fp_x_share_roll13 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 12),
         rec_fp_x_share_roll13 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 12),
         total_fp_x_share_roll13 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 12),
         pass_pacr_roll13 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 12),
         pass_comp_rate_roll13 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 12),
         pass_td_rate_roll13 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 12),
         pass_yac_rate_roll13 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 12),
         pass_adot_roll13 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 12),
         rec_adot_roll13 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 12),
         
         rec_racr_roll14 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 13),
         rec_tar_share_roll14 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 13),
         rec_ay_share_roll14 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 13),
         rec_wopr_roll14 = 1.5*clean_pre2006(Season,rec_tar_share_roll14) + 0.7*rec_ay_share_roll14,
         rec_ypt_roll14 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 13),
         rec_comp_rate_roll14 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 13),
         rec_td_rate_roll14 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 13),
         rec_yptpa_roll14 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 13),
         rec_yac_rate_roll14 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 13),
         rush_ypc_roll14 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 13),
         rush_td_rate_roll14 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 13),
         rush_fp_share_roll14 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 13),
         rec_fp_share_roll14 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 13),
         total_fp_share_roll14 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 13),
         rush_fp_x_share_roll14 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 13),
         rec_fp_x_share_roll14 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 13),
         total_fp_x_share_roll14 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 13),
         pass_pacr_roll14 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 13),
         pass_comp_rate_roll14 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 13),
         pass_td_rate_roll14 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 13),
         pass_yac_rate_roll14 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 13),
         pass_adot_roll14 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 13),
         rec_adot_roll14 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 13),
         
         rec_racr_roll15 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 14),
         rec_tar_share_roll15 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 14),
         rec_ay_share_roll15 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 14),
         rec_wopr_roll15 = 1.5*clean_pre2006(Season,rec_tar_share_roll15) + 0.7*rec_ay_share_roll15,
         rec_ypt_roll15 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 14),
         rec_comp_rate_roll15 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 14),
         rec_td_rate_roll15 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 14),
         rec_yptpa_roll15 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 14),
         rec_yac_rate_roll15 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 14),
         rush_ypc_roll15 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 14),
         rush_td_rate_roll15 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 14),
         rush_fp_share_roll15 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 14),
         rec_fp_share_roll15 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 14),
         total_fp_share_roll15 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 14),
         rush_fp_x_share_roll15 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 14),
         rec_fp_x_share_roll15 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 14),
         total_fp_x_share_roll15 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 14),
         pass_pacr_roll15 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 14),
         pass_comp_rate_roll15 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 14),
         pass_td_rate_roll15 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 14),
         pass_yac_rate_roll15 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 14),
         pass_adot_roll15 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 14),
         rec_adot_roll15 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 14),
         
         rec_racr_roll16 = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = 15),
         rec_tar_share_roll16 = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = 15),
         rec_ay_share_roll16 = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = 15),
         rec_wopr_roll16 = 1.5*clean_pre2006(Season,rec_tar_share_roll16) + 0.7*rec_ay_share_roll16,
         rec_ypt_roll16 = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = 15),
         rec_comp_rate_roll16 = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = 15),
         rec_td_rate_roll16 = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = 15),
         rec_yptpa_roll16 = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = 15),
         rec_yac_rate_roll16 = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = 15),
         rush_ypc_roll16 = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = 15),
         rush_td_rate_roll16 = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = 15),
         rush_fp_share_roll16 = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = 15),
         rec_fp_share_roll16 = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = 15),
         total_fp_share_roll16 = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = 15),
         rush_fp_x_share_roll16 = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = 15),
         rec_fp_x_share_roll16 = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = 15),
         total_fp_x_share_roll16 = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = 15),
         pass_pacr_roll16 = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = 15),
         pass_comp_rate_roll16 = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = 15),
         pass_td_rate_roll16 = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = 15),
         pass_yac_rate_roll16 = slide2_dbl(pass_yac, clean_pre2006(Season,pass_yd), ~get_rate(.x,.y), .before = 15),
         pass_adot_roll16 = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = 15),
         rec_adot_roll16 = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = 15),
         
         rec_racr_career = slide2_dbl(clean_pre2006(Season,rec_yd), rec_ay, ~get_rate(.x,.y), .before = Inf),
         rec_tar_share_career = slide2_dbl(rec_tar, pass_att_team, ~get_rate(.x,.y), .before = Inf),
         rec_ay_share_career = slide2_dbl(rec_ay, pass_ay_team, ~get_rate(.x,.y), .before = Inf),
         rec_wopr_career = 1.5*clean_pre2006(Season,rec_tar_share_career) + 0.7*rec_ay_share_career,
         rec_ypt_career = slide2_dbl(rec_yd, rec_tar, ~get_rate(.x,.y), .before = Inf),
         rec_comp_rate_career = slide2_dbl(rec_comp, rec_tar, ~get_rate(.x,.y), .before = Inf),
         rec_td_rate_career = slide2_dbl(rec_td, rec_comp, ~get_rate(.x,.y), .before = Inf),
         rec_yptpa_career = slide2_dbl(rec_yd, pass_att_team, ~get_rate(.x,.y), .before = Inf),
         rec_yac_rate_career = slide2_dbl(rec_yac, rec_yd, ~get_rate(.x,.y), .before = Inf),
         rush_ypc_career = slide2_dbl(rush_yd, rush_att, ~get_rate(.x,.y), .before = Inf),
         rush_td_rate_career = slide2_dbl(rush_td, rush_att, ~get_rate(.x,.y), .before = Inf),
         rush_fp_share_career = slide2_dbl(rush_fp, rush_fp_team, ~get_rate(.x,.y), .before = Inf),
         rec_fp_share_career = slide2_dbl(rec_fp, rec_fp_team, ~get_rate(.x,.y), .before = Inf),
         total_fp_share_career = slide2_dbl(rec_fp + rush_fp, total_fp_team, ~get_rate(.x,.y), .before = Inf),
         rush_fp_x_share_career = slide2_dbl(rush_fp_x, rush_fp_team_x, ~get_rate(.x,.y), .before = Inf),
         rec_fp_x_share_career = slide2_dbl(rec_fp_x, rec_fp_team_x, ~get_rate(.x,.y), .before = Inf),
         total_fp_x_share_career = slide2_dbl(rec_fp_x + clean_pre2006(Season, rush_fp_x), total_fp_team_x, ~get_rate(.x,.y), .before = Inf),
         pass_pacr_career = slide2_dbl(clean_pre2006(Season,pass_yd), pass_ay, ~get_rate(.x,.y), .before = Inf),
         pass_comp_rate_career = slide2_dbl(pass_comp, pass_att, ~get_rate(.x,.y), .before = Inf),
         pass_td_rate_career = slide2_dbl(pass_td, pass_comp, ~get_rate(.x,.y), .before = Inf),
         pass_yac_rate_career = slide2_dbl(pass_yac, clean_pre2006(Season,clean_pre2006(Season,pass_yd)), ~get_rate(.x,.y), .before = Inf),
         pass_adot_career = slide2_dbl(clean_pre2006(Season,pass_ay), pass_att, ~get_rate(.x,.y), .before = Inf),
         rec_adot_career = slide2_dbl(clean_pre2006(Season,rec_ay), rec_tar, ~get_rate(.x,.y), .before = Inf),
         
         game_number = row_number(),
         across(.cols = where(is.numeric) & contains("career") & !contains("team"),
                .fns = ~lag(.x, 16),
                .names = "{.col}_lag16"),
         
         across(.cols = where(is.numeric) & contains("lag16") & 
                  !(contains(c("racr","share","wopr","ypt","rate","yptpa","ypc","pacr","adot"))),
                .fns = ~./lag(game_number,16)*16),
         
         rush_fp_breakout16 = rush_fp_roll16 - rush_fp_career_lag16,          
         rush_yd_breakout16 = rush_yd_roll16 - rush_yd_career_lag16,         
         rush_att_breakout16 = rush_att_roll16 - rush_att_career_lag16,         
         rush_td_breakout16 = rush_td_roll16 - rush_td_career_lag16,         
         rush_yd_x_breakout16 = rush_yd_x_roll16 - rush_yd_x_career_lag16,        
         rush_td_x_breakout16 = rush_td_x_roll16 - rush_td_x_career_lag16,       
         rush_fp_x_breakout16 = rush_fp_x_roll16 - rush_fp_x_career_lag16,        
         rec_fp_breakout16 = rec_fp_roll16 - rec_fp_career_lag16,          
         rec_yd_breakout16 = rec_yd_roll16 - rec_yd_career_lag16,           
         rec_td_breakout16 = rec_td_roll16 - rec_td_career_lag16,          
         rec_ay_breakout16 = rec_ay_roll16 - rec_ay_career_lag16,           
         rec_tar_breakout16 = rec_tar_roll16 - rec_tar_career_lag16,         
         rec_yac_breakout16 = rec_yac_roll16 - rec_yac_career_lag16,          
         rec_comp_breakout16 = rec_comp_roll16 - rec_comp_career_lag16,        
         rec_comp_x_breakout16 = rec_comp_x_roll16 - rec_comp_x_career_lag16,       
         rec_yd_x_breakout16 = rec_yd_x_roll16 - rec_yd_x_career_lag16,        
         rec_fp_x_breakout16 = rec_fp_x_roll16 - rec_fp_x_career_lag16,         
         rec_td_x_breakout16 = rec_td_x_roll16 - rec_td_x_career_lag16,        
         pass_att_breakout16 = pass_att_roll16 - pass_att_career_lag16,         
         pass_fp_breakout16 = pass_fp_roll16 - pass_fp_career_lag16,         
         pass_yd_breakout16 = pass_yd_roll16 - pass_yd_career_lag16,          
         pass_td_breakout16 = pass_td_roll16 - pass_td_career_lag16,         
         pass_yac_breakout16 = pass_yac_roll16 - pass_yac_career_lag16,         
         pass_comp_breakout16 = pass_comp_roll16 - pass_comp_career_lag16,       
         pass_ay_breakout16 = pass_ay_roll16 - pass_ay_career_lag16,          
         pass_comp_x_breakout16 = pass_comp_x_roll16 - pass_comp_x_career_lag16,     
         pass_yd_x_breakout16 = pass_yd_x_roll16 - pass_yd_x_career_lag16,        
         pass_td_x_breakout16 = pass_td_x_roll16 - pass_td_x_career_lag16,       
         pass_fp_x_breakout16 = pass_fp_x_roll16 - pass_fp_x_career_lag16,        
         total_fp_breakout16 = total_fp_roll16 - total_fp_career_lag16,         
         total_fp_x_breakout16 = total_fp_x_roll16 - total_fp_x_career_lag16,      
         total_fp_diff_breakout16 = total_fp_diff_roll16 - total_fp_diff_career_lag16,    
         total_yd_breakout16 = total_yd_roll16 - total_yd_career_lag16,        
         total_yd_x_breakout16 = total_yd_x_roll16 - total_yd_x_career_lag16,       
         total_yd_diff_breakout16 = total_yd_diff_roll16 - total_yd_diff_career_lag16,   
         total_td_breakout16 = total_td_roll16 - total_td_career_lag16,         
         total_td_x_breakout16 = total_td_x_roll16 - total_td_x_career_lag16,      
         total_td_diff_breakout16 = total_td_diff_roll16 - total_td_diff_career_lag16,    
         rush_fp_diff_breakout16 = rush_fp_diff_roll16 - rush_fp_diff_career_lag16,    
         rush_yd_diff_breakout16 = rush_yd_diff_roll16 - rush_yd_diff_career_lag16,     
         rush_td_diff_breakout16 = rush_td_diff_roll16 - rush_td_diff_career_lag16,    
         rec_fp_diff_breakout16 = rec_fp_diff_roll16 - rec_fp_diff_career_lag16,      
         rec_yd_diff_breakout16 = rec_yd_diff_roll16 - rec_yd_diff_career_lag16,     
         rec_td_diff_breakout16 = rec_td_diff_roll16 - rec_td_diff_career_lag16,      
         rec_comp_diff_breakout16 = rec_comp_diff_roll16 - rec_comp_diff_career_lag16,   
         pass_fp_diff_breakout16 = pass_fp_diff_roll16 - pass_fp_diff_career_lag16,     
         pass_yd_diff_breakout16 = pass_yd_diff_roll16 - pass_yd_diff_career_lag16,    
         pass_td_diff_breakout16 = pass_td_diff_roll16 - pass_td_diff_career_lag16,     
         pass_comp_diff_breakout16 = pass_comp_diff_roll16 - pass_comp_diff_career_lag16,  
         parlay_td_breakout16 = parlay_td_roll16 - parlay_td_career_lag16,        
         rec_racr_breakout16 = rec_racr_roll16 - rec_racr_career_lag16,         
         rec_tar_share_breakout16 = rec_tar_share_roll16 - rec_tar_share_career_lag16,   
         rec_ay_share_breakout16 = rec_ay_share_roll16 - rec_ay_share_career_lag16,     
         rec_wopr_breakout16 = rec_wopr_roll16 - rec_wopr_career_lag16,        
         rec_ypt_breakout16 = rec_ypt_roll16 - rec_ypt_career_lag16,          
         rec_comp_rate_breakout16 = rec_comp_rate_roll16 - rec_comp_rate_career_lag16,   
         rec_td_rate_breakout16 = rec_td_rate_roll16 - rec_td_rate_career_lag16,      
         rec_yptpa_breakout16 = rec_yptpa_roll16 - rec_yptpa_career_lag16,       
         rec_yac_rate_breakout16 = rec_yac_rate_roll16 - rec_yac_rate_career_lag16,     
         rush_ypc_breakout16 = rush_ypc_roll16 - rush_ypc_career_lag16,        
         rush_td_rate_breakout16 = rush_td_rate_roll16 - rush_td_rate_career_lag16,     
         rush_fp_share_breakout16 = rush_fp_share_roll16 - rush_fp_share_career_lag16,   
         rec_fp_share_breakout16 = rec_fp_share_roll16 - rec_fp_share_career_lag16,     
         total_fp_share_breakout16 = total_fp_share_roll16 - total_fp_share_career_lag16,  
         rush_fp_x_share_breakout16 = rush_fp_x_share_roll16 - rush_fp_x_share_career_lag16,  
         rec_fp_x_share_breakout16 = rec_fp_x_share_roll16 - rec_fp_x_share_career_lag16,  
         total_fp_x_share_breakout16 = total_fp_x_share_roll16 - total_fp_x_share_career_lag16, 
         pass_pacr_breakout16 = pass_pacr_roll16 - pass_pacr_career_lag16,       
         pass_comp_rate_breakout16 = pass_comp_rate_roll16 - pass_comp_rate_career_lag16,   
         pass_td_rate_breakout16 = pass_td_rate_roll16 - pass_td_rate_career_lag16,    
         pass_yac_rate_breakout16 = pass_yac_rate_roll16 - pass_yac_rate_career_lag16,    
         pass_adot_breakout16 = pass_adot_roll16 - pass_adot_career_lag16,       
         rec_adot_breakout16 = rec_adot_roll16 - rec_adot_career_lag16,

         across(.cols = c("Week","Season"),
                .fns = as.factor)) %>% 
  ungroup() %>% 
  left_join(ecr_archive_ovr, by = c("Season" = "year", "Week" = "week", "sportradar_id" = "sportsdata_id", "Pos" = "pos")) %>% 
  left_join(ecr_archive_pos, by = c("Season" = "year", "Week" = "week", "sportradar_id" = "sportsdata_id", "Pos" = "pos")) %>%
  mutate(across(.cols = where(is.numeric),
                .fns = ~replace_na(.x, 0)),
         ecr_pos = as.numeric(ecr_pos),
         ecr_ovr = as.numeric(ecr_ovr),
         pos_group = case_when(Pos %in% c('WR','TE') ~ 'Rec',
                             TRUE ~ Pos)) %>% 
  arrange(gsis_game_id, Team, pos_group, -total_fp_x_roll4) %>% 
  group_by(gsis_game_id, Team, pos_group) %>% 
  mutate(teammate_ep_rank = row_number(),
         teammate_ep_gap_to_better = total_fp_x_roll4 - lag(total_fp_x_roll4),
         teammate_ep_gap_to_next = total_fp_x_roll4 - lead(total_fp_x_roll4),
         teammate_ep_gap_to_best = total_fp_x_roll4 - max(total_fp_x_roll4)) %>% 
  ungroup()

write_feather(features_wide, "features_wide.ftr")



# Predict ECR -------------------------------------------------------------
get_ecr_ovr <- function(df){
  
  earth_spec <- mars(
    num_terms = 20,
    prod_degree =  2) %>%
    set_engine("earth", varmod.method = "gam", nfold = 2, ncross = 4, pmethod = "cv", thresh = 0.005) %>%
    set_mode("regression")
  
  df_train <- df %>% 
    filter(!is.na(ecr_ovr))
  
  earth_rec <- recipe(ecr_ovr ~ ., data = df_train) %>%
    update_role(week_season, Team, gsis_game_id, Name, gsis_id, sportradar_id, ecr_pos, new_role = "id") %>%
    step_dummy(c(Season, Week, posteam_type), one_hot = TRUE) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(all_predictors()) 
  
  earth_wf <- workflow() %>%
    add_model(earth_spec) %>%
    add_recipe(earth_rec)

  ecr_ovr_fit <- fit(earth_wf, data = df_train)

  print(summary(ecr_ovr_fit$fit$fit$fit))

  predict(ecr_ovr_fit, new_data = df)
}

get_ecr_pos <- function(df){
  
  earth_spec <- mars(
    num_terms = 20,
    prod_degree =  2) %>%
    set_engine("earth", varmod.method = "gam", nfold = 2, ncross = 4, pmethod = "cv", thresh = 0.005) %>%
    set_mode("regression")

  df_train <- df %>% 
    filter(!is.na(ecr_pos))
  
  earth_rec <- recipe(ecr_pos ~ ., data = df_train) %>%
    update_role(week_season, Team, gsis_game_id, Name, gsis_id, sportradar_id, ecr_ovr, new_role = "id") %>%
    step_dummy(c(Season, Week, posteam_type), one_hot = TRUE) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(all_predictors())
  
  earth_wf <- workflow() %>%
    add_model(earth_spec) %>%
    add_recipe(earth_rec)
  
  ecr_pos_fit <- fit(earth_wf, data = df_train)
  
  print(summary(ecr_pos_fit$fit$fit$fit))

  predict(ecr_pos_fit, new_data = df)
}

clean_ecr <- function(x){
  case_when(x < 1 ~ 1,
            x > 500 ~ 500,
            TRUE ~ x)
}

feature_ecr <- features_wide %>%
  mutate(across(contains("teammate"), ~replace_na(.x, 0)),
         across(contains("teammate"), ~round(.x, 2))) %>% 
  filter(as.character.numeric_version(Season) >= 2006) %>%
  group_by(Pos) %>% 
  nest() %>%
  ungroup() %>% 
  mutate(ecr_ovr_pred = map(data, ~get_ecr_ovr(.x)),
         ecr_pos_pred = map(data, ~get_ecr_pos(.x))) %>% 
  unnest(cols = c(data, ecr_ovr_pred, ecr_pos_pred),
         names_repair = "unique")

feature_ecr2 <- feature_ecr %>% 
  rename(ecr_pos_pred = `.pred...2043`,
         ecr_ovr_pred = `.pred...2044`) %>% 
  
  mutate(ecr_ovr_combo = if_else(is.na(ecr_ovr), ecr_ovr_pred, ecr_ovr),
         ecr_pos_combo = if_else(is.na(ecr_pos), ecr_pos_pred, ecr_pos),
         across(.cols = contains("ecr"),
                .fn = clean_ecr)) %>%
  arrange(gsis_id, gsis_game_id) %>% 
  group_by(gsis_id, Season) %>% 
  mutate(across(.cols = where(is.numeric) & contains("ecr") & !contains("season") ,
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = Inf),
                .names = "{.col}_season")) %>% 
  ungroup() %>% 
  group_by(gsis_id) %>% 
  mutate(across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 1),
                .names = "{.col}_roll2"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 2),
                .names = "{.col}_roll3"),         
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 3),
                .names = "{.col}_roll4"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 4),
                .names = "{.col}_roll5"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 5),
                .names = "{.col}_roll6"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 6),
                .names = "{.col}_roll7"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 7),
                .names = "{.col}_roll8"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 8),
                .names = "{.col}_roll9"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 9),
                .names = "{.col}_roll10"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 10),
                .names = "{.col}_roll11"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 11),
                .names = "{.col}_roll12"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 12),
                .names = "{.col}_roll13"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 13),
                .names = "{.col}_roll14"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 14),
                .names = "{.col}_roll15"),  
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 15),
                .names = "{.col}_roll16"),
         across(.cols = where(is.numeric) & contains("ecr") & !contains("roll") & !contains("season"),
                .fns = ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = Inf),
                .names = "{.col}_career")) %>% 
  ungroup() %>%

  arrange(gsis_game_id, Team, pos_group, ecr_ovr_combo) %>% 
  group_by(gsis_game_id, Team, pos_group) %>%
  mutate(teammate_ecr_rank = row_number(),
         teammate_ecr_gap_to_better = lag(ecr_ovr_combo) - ecr_ovr_combo,
         teammate_ecr_gap_to_next = lead(ecr_ovr_combo) - ecr_ovr_combo,
         teammate_ecr_gap_to_best = min(ecr_ovr_combo) - ecr_ovr_combo,
         across(contains("teammate"), ~replace_na(.x, 0)),
         across(contains("teammate"), ~round(.x, 2))) %>%
  ungroup() %>% 
  select(-pos_group)


write_arrow(feature_ecr2, "feature_mart.pdata")
ep_feature_mart <- read_arrow("feature_mart.pdata")

feature_ecr2 %>% 
  filter(Name == "Tyreek Hill") %>% 
  ggplot(aes(x=Week, y = ecr_pos_pred)) +
  geom_point() +
  geom_smooth() +
  facet_wrap("Season")

temp <- get_ecr_ovr(ep_feature_mart %>% filter(Pos == "RB", !is.na(ecr_ovr)) %>% select(-Pos) %>%  head(100))


new_DF <- ep_pivot %>% filter_all(any_vars(is.na(.))) 
new_DF <- ep_pivot %>% filter_all(any_vars(is.infinite(.))) 

df %>% 
  filter_all(all_vars(!is.infinite(.)))

colnames(temp)[colSums(is.na(temp)) > 0]


ep_longer <- ep_pivot %>%
  group_by(gsis_id, gsis_game_id, Pos) %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "metric",
               values_to = "value")

library(skimr)
temp <- ep_pivot %>% select(contains("fp_x_share"))
temp <- ep_pivot %>% select(!where(is.numeric))

skim(temp)

  
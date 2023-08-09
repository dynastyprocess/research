library(tidyverse)
require(httr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0'
)

res <- httr::GET(url = 'https://api.underdogfantasy.com/beta/v3/over_under_lines',
                 httr::add_headers(.headers=headers))
json_obj <- jsonlite::parse_json(content(res, type = "text", encoding = "UTF-8"))


underdog_props_wide <- 
  tibble(ou_lines = json_obj[["over_under_lines"]]) %>% 
  unnest_wider(col = ou_lines) %>% 
  unnest_wider(col = over_under, names_sep = ".") %>% 
  unnest_wider(col = "over_under.appearance_stat", names_sep = ".") %>% 
  transmute(
    market_site = "underdog",
    
    merge_name = nflreadr::clean_player_names(str_extract(over_under.title, "^(.+?)(?=(( Rushing)|( Rece)|( Pass)|( Comp)))")),
    # merge_name2 = 
    #   nflreadr::clean_player_names(
    #     str_trim(
    #       str_remove(over_under.title,
    #                  glue::glue("{over_under.appearance_stat.display_stat} O/U")))),
    market_name = case_when(over_under.appearance_stat.stat == "receiving_rec" ~ "receptions",
                            over_under.appearance_stat.stat == "receiving_yds" ~ "receiving_yards",
                            over_under.appearance_stat.stat == "rushing_att" ~ "rushing_attempts",
                            over_under.appearance_stat.stat == "rushing_yds" ~ "rushing_yards",
                            over_under.appearance_stat.stat == "passing_yds" ~ "passing_yards",
                            over_under.appearance_stat.stat == "passing_att" ~ "passing_attempts",
                            over_under.appearance_stat.stat == "passing_comps" ~ "passing_completions",
                            
                            over_under.appearance_stat.stat == "rush_rec_yds" ~ "rushing_receiving_yards",
                            TRUE ~ "error"),
    market_line = as.numeric(stat_value),
    alt_flag = FALSE,
    over = 0.5,
    under = 0.5) %>% 
  filter(market_name != "error",
         !is.na(merge_name))

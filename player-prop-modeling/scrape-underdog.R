
underdog_url <- "https://underdogfantasy.com/pick-em/higher-lower/nfl"

scrape_url <- rvest::read_html(underdog_url)

underdog_link <- "https://api.underdogfantasy.com/beta/v3/over_under_lines"


require(httr)

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0',
  `Accept` = 'application/json',
  `Accept-Language` = 'en-US,en;q=0.5',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Client-Type` = 'web',
  `Client-Version` = '202210241305',
  `Client-Device-Id` = '98d76b6c-2868-4950-95d9-f5ae233b7216',
  `Referring-Link` = 'gid://wonder/PromoCode/9a1a3c84-40e5-485c-8987-0bbbdcae95e3',
  `Client-Request-Id` = 'aed6e542-4be5-43e0-8b86-349e4eed8b27',
  `Authorization` = 'Bearer eyJhbGciOiJIUzI1NiJ9.eyJqdGkiOiIyNmFjY2U1MC1iZmNlLTQyZmEtYTFjNi03OWMyZjUzYTA5NmMiLCJzdWIiOiIxNjQyMTQyYS02MWVmLTRlNDQtOTVjMi0yMTRiYTRmNjk4YjAiLCJzY3AiOiJ1c2VyIiwiYXVkIjpudWxsLCJpYXQiOjE2NjY4MTk0NDMsImV4cCI6MTY2OTQ0OTE4OX0.xsus5qC7WqJAuU-GbfsDUfQ_KeewlBLhDse7mP0FL-g',
  `Origin` = 'https://underdogfantasy.com',
  `Connection` = 'keep-alive',
  `Referer` = 'https://underdogfantasy.com/',
  `Sec-Fetch-Dest` = 'empty',
  `Sec-Fetch-Mode` = 'cors',
  `Sec-Fetch-Site` = 'same-site',
  `If-None-Match` = 'W/bf47f238e29b806c190f108e8fe7787d',
  `TE` = 'trailers'
)

res <- httr::GET(url = 'https://api.underdogfantasy.com/beta/v3/over_under_lines', httr::add_headers(.headers=headers))
json_obj2 <- jsonlite::parse_json(content(res, type = "text"))


underdog_obj <- 
  tibble(ou_lines = json_obj2[["over_under_lines"]]) %>% 
  unnest_wider(col = ou_lines) %>% 
  unnest_wider(col = over_under, names_sep = ".") %>% 
  unnest_wider(col = "over_under.appearance_stat", names_sep = ".") %>% 
  mutate(cleaned_name = str_trim(str_remove(over_under.title,
                                            glue::glue("{over_under.appearance_stat.display_stat} O/U"))),
         merge_name = clean_player_names(cleaned_name),
         stat_value = as.numeric(stat_value)
  ) %>% 
  select(merge_name, stat = over_under.appearance_stat.stat, stat_value)

underdog_receptions <- 
  underdog_obj %>% 
  filter(stat == "receiving_rec") %>% 
  left_join(rec_train_preds_long, by = c("merge_name", "stat_value" = "reception_pred")) %>% 
  transmute(
    merge_name,
    position,
    team,
    rank,
    stat,
    stat_value,
    under_prob = under_prob - value,
    over_prob = over_prob,
    push_prob = value,
    best_ev = pmax(under_prob, over_prob),
    over_under = if_else(best_ev == over_prob, "over", "under"))

underdog_receptions2 <- 
  underdog_obj %>% 
  filter(stat == "receiving_rec") %>% 
  left_join(rec_train_preds_long, by = c("merge_name", "stat_value" = "reception_line")) %>% 
  transmute(
    merge_name,
    position,
    team,
    rank,
    stat,
    stat_value,
    under_prob = under_prob,
    over_prob = over_prob,
    best_ev = pmax(under_prob, over_prob),
    over_under = if_else(best_ev == over_prob, "over", "under"))
  
underdog_rec_yards <- 
  underdog_obj %>% 
  filter(stat == "receiving_yds") %>% 
  left_join(rec_yards_preds_long, by = c("merge_name", "stat_value" = "rec_yards_line")) %>% 
  transmute(
    merge_name,
    position,
    team,
    rank,
    stat,
    stat_value,
    under_prob,
    over_prob,
    best_ev = pmax(under_prob, over_prob),
    over_under = if_else(best_ev == over_prob, "over", "under")
  )

underdog_rush_att <- 
  underdog_obj %>% 
  filter(stat == "rushing_att") %>% 
  left_join(rush_train_preds_long, by = c("merge_name", "stat_value" = "rush_attempt_pred")) %>% 
  transmute(
    merge_name,
    position,
    team,
    rank,
    stat,
    stat_value,
    under_prob = under_prob - value,
    over_prob = over_prob,
    push_prob = value,
    best_ev = pmax(under_prob, over_prob),
    over_under = if_else(best_ev == over_prob, "over", "under"))

underdog_rush_att2 <- 
  underdog_obj %>% 
  filter(stat == "rushing_att") %>% 
  left_join(rush_train_preds_long, by = c("merge_name", "stat_value" = "rush_attempt_line")) %>% 
  transmute(
    merge_name,
    position,
    team,
    rank,
    stat,
    stat_value,
    under_prob = under_prob,
    over_prob = over_prob,
    best_ev = pmax(under_prob, over_prob),
    over_under = if_else(best_ev == over_prob, "over", "under"))

underdog_rush_yards <- 
  underdog_obj %>% 
  filter(stat == "rushing_yds") %>% 
  left_join(rush_yards_preds_long, by = c("merge_name", "stat_value" = "rush_yard_line")) %>% 
  transmute(
    merge_name,
    position,
    team,
    rank,
    stat,
    stat_value,
    under_prob,
    over_prob,
    best_ev = pmax(under_prob, over_prob),
    over_under = if_else(best_ev == over_prob, "over", "under")
  )

underdog_bets <- 
  underdog_receptions %>% 
  bind_rows(underdog_rec_yards) %>% 
  bind_rows(underdog_receptions2) %>% 
  bind_rows(underdog_rush_att) %>% 
  bind_rows(underdog_rush_att2) %>% 
  bind_rows(underdog_rush_yards) %>% 
  arrange(-best_ev)
  

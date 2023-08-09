require(httr)
library(googlesheets4)

prop_bet_tracker <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1Qu_tkaU2jy-cEzC6knlxS7UYX7GD8bR74JfpovMZIJs/edit#gid=200549437",
           # col_names = c("Player","Drop","Acquired","Salary","Class","Length"),
           sheet = "Player Props") %>% 
  mutate(bet_type = case_when(str_detect(BetType, "Rec") ~ "receiving",
                              str_detect(BetType, "Rush") ~ "rushing",
                              TRUE ~ "other"),
         line_type = case_when(BetType == "Total Receptions" ~ "receptions",
                               BetType == "Total Receiving Yards" ~ "receiving_yards",
                               BetType == "Total Rushing Attempts" ~ "rushing_attempts",
                               TRUE ~ "other"))

headers = c(
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0',
  `Accept` = 'application/json, text/plain, */*',
  `Accept-Language` = 'en-US,en;q=0.5',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Origin` = 'https://unabated.com',
  `Connection` = 'keep-alive',
  `Referer` = 'https://unabated.com/',
  `Sec-Fetch-Dest` = 'empty',
  `Sec-Fetch-Mode` = 'cors',
  `Sec-Fetch-Site` = 'same-site',
  `TE` = 'trailers'
)

params = list(
  `uuid` = '3fbe6d29-0f32-4322-9ffa-8224c399f1a9'
)

res <-
  httr::GET(url = 'https://content.unabated.com/markets/b_playerprops.json',
            #httr::add_headers(.headers = headers),
            query = params)


json_obj <- jsonlite::parse_json(content(res, type = "text"))

props <- 
  tibble(pregame = json_obj[["propsPeopleEvents"]][["lg1:pt1:pregame"]]) %>% 
  unnest_wider(pregame) %>% 
  unnest_longer(propsMarketSourcesLines) %>%
  unnest_longer(propsMarketSourcesLines, names_repair = "minimal") %>% 
  unnest_wider(propsMarketSourcesLines)

people <- 
  tibble(people = json_obj[["people"]]) %>% 
  unnest_wider(col = people)

markets <- 
  tibble(markets = json_obj[["marketSources"]]) %>%
  unnest_wider(col = markets)
  
teams <- 
  tibble(markets = json_obj[["teams"]]) %>%
  unnest_wider(col = markets)

reception_prop <- props %>% 
  filter(propsMarketSourcesLines_id == "bt15") %>%
  left_join(people, by = c("personId" = "id")) %>% 
  distinct(firstName, lastName, position, points, price, marketId, marketLineId) %>%
  group_by(firstName, lastName, position) %>%
  mutate(over_under = if_else(marketId == min(marketId), "market_over", "market_under"),
         odds_prob = odds.converter::odds.us2prob(price),
         merge_name = clean_player_names(paste(firstName, lastName))) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(merge_name, position, points),
              values_from = odds_prob,
              names_from = over_under,
              values_fn = min)

rec_bets <- 
  reception_prop %>% 
  left_join(rec_train_preds_long, by = c("merge_name", "position", "points" = "reception_line")) %>% 
  mutate(under_ev = under_prob - market_under,
         over_ev = over_prob - market_over,
         best_ev = pmax(under_ev, over_ev),
         over_under = if_else(best_ev == over_ev, "over", "under"),
         expected_odds_prob = if_else(over_under == "over", over_prob, under_prob),
         market_odds_prob = if_else(over_under == "over", market_over, market_under),
         
         market_odds_frac = (1 / (market_odds_prob)) - 1,
         kelly_frac = expected_odds_prob-((1-expected_odds_prob)/market_odds_frac)) %>% 
  transmute(
    merge_name,
    position,
    team,
    line_type = "receptions",
    bet_type = "receiving",
    over_under,
    points,
    market_odds = odds.prob2us(market_odds_prob),
    expected_odds = odds.prob2us(expected_odds_prob),
    kelly_frac,
    best_ev,
    rank,
    ) %>% 
  mutate(across(.cols = where(is.numeric),
                             .fn = ~round(.x, 3)))

rec_yards_prop <- props %>% 
  filter(propsMarketSourcesLines_id == "bt16") %>%
  left_join(people, by = c("personId" = "id")) %>% 
  distinct(firstName, lastName, position, points, price, marketId, marketLineId) %>%
  group_by(firstName, lastName, position) %>%
  mutate(over_under = if_else(marketId == min(marketId), "market_over", "market_under"),
         odds_prob = odds.converter::odds.us2prob(price),
         merge_name = clean_player_names(paste(firstName, lastName))) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(merge_name, position, points),
              values_from = odds_prob,
              names_from = over_under,
              values_fn = min)

rec_yards_bets <- 
  rec_yards_prop %>% 
  left_join(rec_yards_preds_long, by = c("merge_name", "position", "points" = "rec_yards_line")) %>% 
  mutate(under_ev = under_prob - market_under,
         over_ev = over_prob - market_over,
         best_ev = pmax(under_ev, over_ev),
         over_under = if_else(best_ev == over_ev, "over", "under"),
         expected_odds_prob = if_else(over_under == "over", over_prob, under_prob),
         market_odds_prob = if_else(over_under == "over", market_over, market_under),
         
         market_odds_frac = (1 / (market_odds_prob)) - 1,
         kelly_frac = expected_odds_prob-((1-expected_odds_prob)/market_odds_frac))  %>% 
  transmute(
    merge_name,
    position,
    team,
    line_type = "receiving_yards",
    bet_type = "receiving",
    over_under,
    points,
    market_odds = odds.prob2us(market_odds_prob),
    expected_odds = odds.prob2us(expected_odds_prob),
    kelly_frac,
    best_ev,
    rank,
  ) %>% 
  mutate(across(.cols = where(is.numeric),
                .fn = ~round(.x, 3)))

rush_att_prop <- props %>% 
  filter(propsMarketSourcesLines_id == "bt11") %>%
  left_join(people, by = c("personId" = "id")) %>% 
  distinct(firstName, lastName, position, points, price, marketId, marketLineId) %>%
  group_by(firstName, lastName, position) %>%
  mutate(over_under = if_else(marketId == min(marketId), "market_over", "market_under"),
         odds_prob = odds.converter::odds.us2prob(price),
         merge_name = clean_player_names(paste(firstName, lastName))) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(merge_name, position, points),
              values_from = odds_prob,
              names_from = over_under,
              values_fn = min)

rush_attempt_bets <- 
  rush_att_prop %>% 
  left_join(rush_train_preds_long, by = c("merge_name", "position", "points" = "rush_attempt_line")) %>% 
  mutate(under_ev = under_prob - market_under,
         over_ev = over_prob - market_over,
         best_ev = pmax(under_ev, over_ev),
         over_under = if_else(best_ev == over_ev, "over", "under"),
         expected_odds_prob = if_else(over_under == "over", over_prob, under_prob),
         market_odds_prob = if_else(over_under == "over", market_over, market_under),
         
         market_odds_frac = (1 / (market_odds_prob)) - 1,
         kelly_frac = expected_odds_prob-((1-expected_odds_prob)/market_odds_frac))  %>% 
  transmute(
    merge_name,
    position,
    team,
    line_type = "rushing_attempts",
    bet_type = "rushing",
    over_under,
    points,
    market_odds = odds.prob2us(market_odds_prob),
    expected_odds = odds.prob2us(expected_odds_prob),
    kelly_frac,
    best_ev,
    rank,
  ) %>% 
  mutate(across(.cols = where(is.numeric),
                .fn = ~round(.x, 3)))

rush_yards_prop <- props %>% 
  filter(propsMarketSourcesLines_id == "bt12") %>%
  left_join(people, by = c("personId" = "id")) %>% 
  distinct(firstName, lastName, position, points, price, marketId, marketLineId) %>%
  group_by(firstName, lastName, position) %>%
  mutate(over_under = if_else(marketId == min(marketId), "market_over", "market_under"),
         odds_prob = odds.converter::odds.us2prob(price),
         merge_name = clean_player_names(paste(firstName, lastName))) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(merge_name, position, points),
              values_from = odds_prob,
              names_from = over_under,
              values_fn = min)

rush_yards_bets <- 
  rush_yards_prop %>% 
  left_join(rush_yards_preds_long, by = c("merge_name", "position", "points" = "rush_yard_line")) %>% 
  mutate(under_ev = under_prob - market_under,
         over_ev = over_prob - market_over,
         best_ev = pmax(under_ev, over_ev),
         over_under = if_else(best_ev == over_ev, "over", "under"),
         expected_odds_prob = if_else(over_under == "over", over_prob, under_prob),
         market_odds_prob = if_else(over_under == "over", market_over, market_under),
         
         market_odds_frac = (1 / (market_odds_prob)) - 1,
         kelly_frac = expected_odds_prob-((1-expected_odds_prob)/market_odds_frac))  %>% 
  transmute(
    merge_name,
    position,
    team,
    line_type = "rushing_yards",
    bet_type = "rushing",
    over_under,
    points,
    market_odds = odds.prob2us(market_odds_prob),
    expected_odds = odds.prob2us(expected_odds_prob),
    kelly_frac,
    best_ev,
    rank) %>% 
  mutate(across(.cols = where(is.numeric),
                .fn = ~round(.x, 3)))

kelly_rank <- 
  rec_bets %>% 
  bind_rows(rec_yards_bets) %>% 
  bind_rows(rush_attempt_bets) %>%  
  group_by(merge_name, position, bet_type) %>% 
  slice_max(kelly_frac, n = 1) %>% 
  ungroup() %>% 
  mutate(kelly_rank = row_number(-kelly_frac)) %>% 
  select(merge_name, position, bet_type, kelly_rank)

bets <- 
  rec_bets %>% 
  bind_rows(rec_yards_bets) %>%
  bind_rows(rush_attempt_bets) %>%
  bind_rows(rush_yards_bets) %>%
  left_join(kelly_rank) %>% 
  arrange(kelly_rank, -kelly_frac) %>%
  mutate(target_bet_size = kelly_frac*18)
  # mutate(target_bet_size = kelly_frac*15)

already_bet <- 
  prop_bet_tracker %>% 
  filter(Week == current_week) %>% 
  select(Player, line_type, bet_type, Points, BetOdds, BetAmount, KellyFrac)

bets_moved <- 
  bets %>% 
  left_join(already_bet, by = c("merge_name" = "Player", "line_type")) %>% 
  mutate(move_type = case_when(KellyFrac>kelly_frac ~ "Got Best Value",
                               KellyFrac<kelly_frac ~ "Better Odds Now",
                               KellyFrac == kelly_frac ~ "Same Odds"),
         kelly_diff = round(kelly_frac - KellyFrac,3))

new_bets <- 
  bets %>% 
  anti_join(already_bet %>% select(Player, bet_type), by = c("merge_name" = "Player", "bet_type")) %>% 
  filter(kelly_frac >= 0.1)

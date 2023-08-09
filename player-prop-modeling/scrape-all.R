source("./fanduel-scraper.R")
source("./draftkings-scraper.R")
source("./caesar-scraper.R")
source("./pointsbet-scraper.R")
source("./betrivers-scraper.R")
source("./betmgm-scraper.R")
source("./scrape-underdog-v2.R")

combine_lines <- 
  draftkings_props_wide %>%
  bind_rows(pointsbet_props_wide) %>% 
  bind_rows(fanduel_props_wide) %>%
  bind_rows(caesar_props_wide) %>%
  bind_rows(betmgm_props_wide) %>%
  bind_rows(underdog_props_wide) %>%
  mutate(merge_name = case_when(merge_name == "D Hilliard" ~ "Dontrell Hilliard",
                                merge_name == "Steven Sims Jr" ~ "Steven Sims",
                                merge_name == "Isaiah Mckenzie" ~ "Isaiah McKenzie",
                                merge_name == "Lamar Jackson (BAL)" ~ "Lamar Jackson",
                                merge_name == "Christian Mccaffrey" ~ "Christian McCaffrey",
                                merge_name == "Aj Dillon" ~ "AJ Dillon",
                                merge_name == "Dandre Swift" ~ "DAndre Swift",
                                merge_name == "Robert Tonyan Jr" ~ "Robert Tonyan",
                                merge_name == "Chig Okonkwo" ~ "Chigoziem Okonkwo",
                                merge_name == "Ceedee Lamb" ~ "CeeDee Lamb",
                                merge_name == "Juju Smith-Schuster" ~ "JuJu Smith-Schuster",
                                merge_name == "Jamarr Chase" ~ "JaMarr Chase",
                                merge_name == "Terry Mclaurin" ~ "Terry McLaurin",
                                merge_name == "Robert" ~ "Robert Tonyan",
                                TRUE ~ merge_name
                                )) %>% 
  filter(merge_name %ni% c())

saveRDS(combine_lines, "./20221209proplines.RDS")

combine_preds <- 
  reception_preds_long %>% 
  bind_rows(rec_yards_preds_long) %>% 
  bind_rows(rush_att_preds_long) %>% 
  bind_rows(rush_yards_preds_long)

combine_lines_preds <- 
  combine_lines %>% 
  left_join(combine_preds, by = c("market_name","market_line", "merge_name")) %>% 
  mutate(under_ev = under_prob - under,
         over_ev = over_prob - over,
         best_ev = pmax(under_ev, over_ev, na.rm = TRUE),
         over_under = if_else(best_ev == over_ev, "over", "under"),
         expected_odds_prob = if_else(over_under == "over", over_prob, under_prob),
         market_odds_prob = if_else(over_under == "over", over, under),
         market_odds_frac = (1 / (market_odds_prob)) - 1,
         kelly_frac = expected_odds_prob-((1-expected_odds_prob)/market_odds_frac)) %>% 
  transmute(market_site,
            merge_name,
            team,
            market_name,
            market_class = case_when(market_name %in% c("rushing_yards", "rushing_attempts") ~ "rushing",
                                     market_name %in% c("receiving_yards", "receptions") ~ "receiving",
                                     market_name %in% c("passing_yards", "passing_attempts", "passing_completions", "passing_touchdowns", "passing_interceptions") ~ "passing",
                                     market_name %in% c("rushing_receiving_yards", "rushing_receiving_touchdowns", "rushing_receiving_yards") ~ "combo",
                                     TRUE ~ "error"),
            market_line,
            alt_flag,
            over_under,
            market_odds_us = round(odds.converter::odds.prob2us(market_odds_prob)),
            expected_odds_prob,
            kelly_frac = round(kelly_frac, 4),
            best_ev = round(best_ev, 3),
            rank = coalesce(rank, rank.x),
            game_id)

# Where are these preds?
# combine_lines_preds %>%
#   filter(is.na(best_ev), market_line %% 1 == 0.5) %>%
#   view()

combine_lines_preds %>% 
  filter(market_site != "underdog",
         market_class != "error"
         # game_id %in% c("2022_12_LAC_ARI", "2022_12_LV_SEA","2022_12_LA_KC","2022_12_NO_SF","2022_12_GB_PHI","2022_12_PIT_IND")
         ) %>%
  # group_by(market_site, merge_name, market_name) %>%
  # slice_max(order_by = kelly_frac, n = 1) %>% 
  # ungroup() %>%
  # group_by(market_site, game_id) %>%
  # slice_max(order_by = kelly_frac, n = 1) %>% 
  # ungroup() %>% 
  # group_by(market_site) %>%
  # slice_max(order_by = kelly_frac, n = 4) %>%
  # mutate(avg_kelly = mean(kelly_frac),
  #        bet_amount = 145 * avg_kelly) %>% 
  # ungroup() %>% 
  # arrange(-avg_kelly) %>% 
  view()


  group_by(market_site) %>% 
  slice_max(order_by = kelly_frac, n =5) %>%
  mutate(site_rank = row_number(-kelly_frac)) %>% 
  ungroup() %>% view()
  pivot_wider(id_cols = c(market_site),
              names_from = site_rank,
              names_glue = "rank{site_rank}",
              values_from = expected_odds_prob) %>% 
  mutate(parlay_odds = rank1*rank2*rank3*rank4*rank5) %>% 
  view()
  
  compare_odds <- function(my_odds, their_odds) {
    odds.converter::odds.us2prob(my_odds) - odds.converter::odds.us2prob(their_odds)
    
  }
  
  calculate_kelly <- function(my_odds, their_odds) {
    market_odds_prob = odds.converter::odds.us2prob(their_odds)
    market_odds_frac = (1 / (market_odds_prob)) - 1
    odds.converter::odds.us2prob(my_odds)-((1-odds.converter::odds.us2prob(my_odds))/market_odds_frac)
  }
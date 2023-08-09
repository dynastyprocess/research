library(tidyverse)

# tan <- function(n_bets){
#   x <- rep(list(c(1L,0L)),n_bets) |> purrr::set_names(glue::glue("bet{seq_len(n_bets)}_win"))
#   tidyr::expand_grid(!!!x)
# }
# 
# 
# max_log_profit <- function(bet_sizes, odds_table) {
# 
#   combined_bets <-
#     odds_table %>%
#     bind_cols(bet_sizes = bet_sizes) %>%
#     mutate(potential_winning = bet_sizes*odds_frac) %>%
#     select(-odds_frac) %>%
#     pivot_wider(values_from = c(potential_winning, expected_odds, bet_sizes),
#                 names_from = bet_name)
# 
#   bet_grid <- tibble()
# 
#   for (i in 1:nrow(odds_table)) {
# 
#     temp_vec <- tibble("bet{i}_win" := c(0,1))
# 
#     bet_grid <-
#       bet_grid %>%
#       full_join(temp_vec, by = character())
# 
#   }
# 
#   bet_grid <-
#     bet_grid %>%
#     bind_cols(combined_bets)
# 
#   for (i in 1:nrow(odds_table)) {
# 
#     odds_col <- glue::glue("expected_odds_bet{i}")
#     bet_win <- glue::glue("bet{i}_win")
# 
#     bet_grid <-
#       bet_grid %>%
#       mutate("bet{i}_prob" := !!!bet_win * !!!odds_col )
#     
#       
#       mutate("bet{i}_prob" := if_else({{bet_win}} == 1, as.numeric({{odds_col}}), 1 - as.numeric({{odds_col}})))
# 
#   }
# 
# 
# 
#       mutate(bet1_prob = if_else(bet1_win == 1, expected_odds_bet1, 1 - expected_odds_bet1),
#              bet2_prob = if_else(bet2_win == 1, expected_odds_bet2, 1 - expected_odds_bet2),
#              bet3_prob = if_else(bet3_win == 1, expected_odds_bet3, 1 - expected_odds_bet3),
#              bet4_prob = if_else(bet4_win == 1, expected_odds_bet4, 1 - expected_odds_bet4),
#              bet1_profit = if_else(bet1_win == 1, potential_winning_bet1, -bet_sizes_bet1),
#              bet2_profit = if_else(bet2_win == 1, potential_winning_bet2, -bet_sizes_bet2),
#              bet3_profit = if_else(bet3_win == 1, potential_winning_bet3, -bet_sizes_bet3),
#              bet4_profit = if_else(bet4_win == 1, potential_winning_bet4, -bet_sizes_bet4),
# 
#              combination_prob = bet1_prob*bet2_prob*bet3_prob*bet4_prob,
#              combination_profit = 1000+bet1_profit+bet2_profit+bet3_profit+bet4_profit,
#              log_ending = log(combination_profit)
# 
# 
#              )
# 
#     return(-weighted.mean(x = bet_grid$log_ending, w = bet_grid$combination_prob))
# 
# }
# 
# optim(par = c(5,5,5,5),
#       fn = max_log_profit,
#       odds_table = odds_table,
#       control = list(trace = TRUE))

odds_table <-
  tibble(bet_name = c("bet1","bet2","bet3", "bet4"),
         expected_odds = c(.76,.75,.73,.52),
         odds_frac = c(0.87,0.95,0.87,0.91))

simulate_bets <- function(bet_sizes, odds_table){
  
  sim_table <- odds_table %>% 
    bind_cols(bet_size = bet_sizes) %>% 
    mutate(bet_outcome = runif(nrow(.)),
           potential_winning = bet_size*odds_frac,
           bet_profit = if_else(expected_odds >= bet_outcome, potential_winning, -bet_size))
  # browser()
  return(sum(sim_table$bet_profit))
}

run_many_sims <- function(number_of_sims = 2000, bet_sizes, odds_table){
  bet_profit <- c()
  for (i in 1:number_of_sims){
    bet_profit <- append(bet_profit, simulate_bets(bet_sizes = bet_sizes,
                                                   odds_table)
    )
    
  }
  return(-log(mean(bet_profit)))
  
}

# run_many_sims(10, bet_sizes = c(1,1,1,1), odds_table = odds_table)

optim(par = c(5,5,5,5),
      fn = run_many_sims,
      odds_table = odds_table,
      number_of_sims = 2000,
      control = list(trace = TRUE))



bet_profit %>% tibble() %>% mutate(
  n.row = row_number(),
  rolling_mean = slider::slide_dbl(.x = `.`,
                                   .f = mean,
                                   .before = Inf)
) %>%
  ggplot(aes(x = n.row, y = rolling_mean)) +
  geom_point() +
  theme_minimal()


# max_log_profit(c(5,5,5,5), odds_table)



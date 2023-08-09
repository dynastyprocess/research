library(ffopportunity)
library(nflreadr)
library(tidyverse)

pbp <- load_pbp(season = 2022)

ep_pbp <- ffopportunity::ep_preprocess(pbp)

ep_pbp_x <- ffopportunity::ep_predict(ep_pbp)

pass_pbp <- ep_pbp_x$pass_df

throwing_short <- 
  pass_pbp %>% 
  filter(as.numeric(down) >= 3) %>% 
  group_by(passer_full_name, passer_player_id, receiver_full_name) %>% 
  summarise(short_of_the_sticks = mean(relative_to_sticks < 0),
            at_least_the_sticks = mean(relative_to_sticks >= 0),
            first_downs = mean(if_else(first_down == 1, 1, 0)),
            first_downs_exp = mean(pass_first_down_exp),
            air_yards = mean(air_yards),
            yards_to_go = mean(ydstogo),
            yac = mean(receiving_yards - air_yards, na.rm = TRUE),
            yac_exp = mean(ifelse(complete_pass == 1, yards_after_catch_exp, NA), na.rm = TRUE),
            completion_pct = mean(if_else(complete_pass == 1, 1, 0)),
            completion_pct_exp = mean(pass_completion_exp),
            
            throws = n()) %>% 
  ungroup() %>% 
  # filter(throws >= 50) %>% 
  transmute(passer_full_name,
            receiver_full_name,
            throws,
            short_of_the_sticks,
            yards_to_go,
            air_yards,
            first_downs,
            first_downs_exp,
            first_down_oe = first_downs - first_downs_exp,
            completion_pct,
            completion_pct_exp,
            comp_pct_oe = completion_pct - completion_pct_exp,
            yac,
            yac_exp,
            yac_oe = yac - yac_exp
  ) %>% 
  mutate(across(.cols = where(is.numeric),
                .fns = ~round(.x, digits = 3))) %>% 
  arrange(-short_of_the_sticks)


throwing_short %>% 
  ggplot(aes(x = short_of_the_sticks, y = first_down_oe, label = passer_full_name)) +
  theme_minimal() +
  geom_point() +
  ggrepel::geom_label_repel()


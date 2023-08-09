library(ffpros)
library(tidyverse)
library(ffscrapr)


# 2021
seasons2 <- 2012:2021
weeks <- 1:17
pages2 <- c(
  "qb",
  "ppr-flex"
)

fp_rankings_history2 <- crossing(pages2, seasons2, weeks) %>%
  mutate(rankings = pmap(
    list(pages2, seasons2, weeks),
    possibly(~fp_rankings(page = ..1, year = ..2, week = ..3),
             tibble())
  )) %>%
  unnest(rankings) %>%
  transmute(
    page_pos =
      str_remove_all(pages2, "cheatsheets|^ppr|\\-") %>%
      toupper() %>%
      str_squish(),
    season = seasons2,
    week = weeks,
    fantasypros_id = as.character(fantasypros_id),
    sportradar_id,
    player_name = dp_cleannames(player_name),
    pos,
    team,
    rank,
    ecr,
    sd
  )

saveRDS(fp_rankings_history2, "data/fantasy_pros_weekly_2012_2021.RDS")


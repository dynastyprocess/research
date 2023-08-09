library(ffpros)
library(tidyverse)
library(ffscrapr)


old_fp2022 <- readRDS("data/fantasy_pros_weekly_2022.RDS")

# 2021
seasons2 <- 2022
weeks <- 13:14
pages2 <- c(
  "qb",
  "ppr-flex"
)

new_fp_rankings <- crossing(pages2, seasons2, weeks) %>%
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

previous_weekly_ranks <- 
  old_fp2022 %>% 
  filter(season == seasons2, week == weeks)

print("Newly Ranked")
new_fp_rankings %>% 
  left_join(previous_weekly_ranks,
            by = c("player_name"),
            suffix = c(".new",".old")) %>% 
  filter(is.na(rank.old))%>% 
  select(team.new, player_name, rank.new)

print("Ruled Out")
previous_weekly_ranks %>% 
  left_join(new_fp_rankings,
            by = c("player_name"),
            suffix = c(".old",".new")) %>% 
  filter(is.na(rank.new)) %>% 
  select(team.old, player_name, rank.old)

print("Changed Rank")
previous_weekly_ranks %>% 
  inner_join(new_fp_rankings,
            by = c("player_name"),
            suffix = c(".old",".new")) %>%
  mutate(rank.diff = rank.old - rank.new) %>% 
  filter(abs(rank.diff) >= 5,
         rank.new <= 250) %>%
  select(team.old, player_name, rank.old, rank.new, rank.diff) %>% 
  view()

write_new_ranks <- 
  new_fp_rankings %>% 
  # filter(player_name %ni% c("Mark Andrews", "Gus Edwards", "Darren Waller", "Damien Harris")) %>%
  bind_rows(old_fp2022 %>% filter(week < min(weeks)))

saveRDS(write_new_ranks, "data/fantasy_pros_weekly_2022.RDS")

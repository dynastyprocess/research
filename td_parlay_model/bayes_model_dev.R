suppressPackageStartupMessages({
  # Data import
  library(arrow)
  library(DBI)
  library(here)
  
  # Data manipulation
  library(tidyverse)
  library(slider)
  library(lubridate)
  
  # Plotting
  library(ggbeeswarm)
  library(ggthemes)
  library(directlabels)
  library(ggridges)
  
  #EDA
  library(skimr)
  library(usemodels)
  library(tidymodels)
  library(vip)
  library(baguette)
  
  library(brms)
  library(tidybayes)
  
  
})

# Import Data -------------------------------------------------------------
setwd(here::here())

memory.size(60000)

ep_feature_mart <- read_arrow("C:/Users/syd23/Documents/DynastyProcess/db/feature_mart.pdata")

td_model_prep <- ep_feature_mart %>%
  filter(as.character.numeric_version(Season) >= 2016, as.character.numeric_version(Season) < 2020, Pos == "WR") %>%
  #select(-(contains("ecr") & !(contains("combo") | contains("teammate")))) %>% 
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id) %>%
  mutate(across(.cols = c(parlay_td, posteam_type, spread_line, total_line, implied_total,
                          ecr_ovr, ecr_pos,
                          teammate_ecr_rank, teammate_ecr_gap_to_better,
                          teammate_ecr_gap_to_next, teammate_ecr_gap_to_best),
                .fns = ~lead(.x), 
                .names = "{.col}_next"),
         across(contains("teammate"), ~replace_na(.x, 0)),
         across(contains("teammate"), ~round(.x, 2)),
         Week = as.numeric(Week),
         week_group = as.factor(case_when(Week > 17 ~ "Playoffs",
                                          TRUE ~ as.character(Week)))) %>% 
  ungroup() %>%
  select(Name, gsis_id, parlay_td_next, ecr_pos_next)

fit1 <- brm(parlay_td_next ~ ecr_pos_next + (1|gsis_id), 
            data = td_model_prep,
            family = bernoulli,
            link = probit,
            iter = 1000,
            chains = 2,
            refresh = 50,
            cores = 3,
            control = list(adapt_delta = 0.80, max_treedepth = 10)
        )

summary(fit1)
plot(fit1)
plot(conditional_effects(fit1, effects = "ecr_pos_next"))

posteriorsimpelmodel1 <- as_tibble(t(posterior_samples(fit1, pars = "ecr_pos_next")[,-c(1:3)]))

posteriorsimpelmodel1 <- posterior_samples(fit1, pars = "ecr_pos_next")


existing_td <- td_model_prep %>% 
  group_by(gsis_id, Name) %>% 
  summarise(td_average = mean(parlay_td_next, na.rm = TRUE),
            games = n()) %>% 
  ungroup() %>% 
  slice_max(games, n = 15)

temp <- posterior_summary(fit1, probs = seq(0.01,1,0.01) #probs = c(.025, .25, 0.5, .75, .975)
                          ) %>%
  as_tibble(rownames = "id") %>% 
  filter(str_detect(id, "Intercept")) %>% 
  mutate(gsis_id = str_sub(id,11,20),
         across(where(is.numeric), ~round(.x,3))) %>% 
  inner_join(existing_td, by = "gsis_id")

temp %>% 
  pivot_longer(cols = contains("Q")) %>% 
  ggplot(aes(x = value, y = reorder(as.factor(Name), Estimate))) +
  xlim(-0.2,0.2) +
  geom_density_ridges(aes(height = ..density..), quantile_lines = TRUE, quantiles = 2) +

  geom_point(data = temp,
             aes(x = Estimate, 
                 y = as.factor(Name)),
             size = 1, 
             col  = "red")

predict16 <- ep_feature_mart %>% 
  group_by(gsis_id) %>% 
  filter(sum(Season == 2019) > 0, Pos == "WR") %>%
  ungroup() %>% 
  arrange(gsis_id, gsis_game_id) %>%
  group_by(gsis_id) %>%
  mutate(across(.cols = c(parlay_td, posteam_type, spread_line, total_line, implied_total,
                          ecr_ovr, ecr_pos,
                          teammate_ecr_rank, teammate_ecr_gap_to_better,
                          teammate_ecr_gap_to_next, teammate_ecr_gap_to_best),
                .fns = ~lead(.x), 
                .names = "{.col}_next"),
         across(contains("teammate"), ~replace_na(.x, 0)),
         across(contains("teammate"), ~round(.x, 2)),
         Week = as.numeric(Week),
         week_group = as.factor(case_when(Week > 17 ~ "Playoffs",
                                          TRUE ~ as.character(Week)))) %>% 
  ungroup() %>%
  filter(gsis_id %in% temp$gsis_id, !is.na(parlay_td_next), !is.na(ecr_pos_next)) %>%
  group_by(gsis_id) %>% 
  filter(gsis_game_id == max(gsis_game_id)) %>% 
  ungroup() %>% 
  select(week_season, Name, gsis_id, parlay_td_next, ecr_pos_next) %>%
  mutate(week16 = predict(fit1, newdata = ., re_formula = NA))

library(tidyverse)
library(skimr)
library(ggpmisc)
library(slider)

games <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/games.csv")

skim(games)

formula <- y ~ x

ggplot(games, aes(x=spread_line, y=result))  +
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)


# Reformat data -----------------------------------------------------------
games_new <- games %>%
  mutate(favored_coach = case_when(spread_line > 0 ~ home_coach,
                                   spread_line < 0 ~ away_coach,
                                   spread_line == 0 ~ away_coach),
         dog_coach = case_when(spread_line > 0 ~ away_coach,
                               spread_line < 0 ~ home_coach,
                               spread_line == 0 ~ home_coach),
         favored_spread = -abs(spread_line),
         favored_result = case_when(home_coach == favored_coach ~ away_score - home_score,
                                    away_coach == favored_coach ~ home_score - away_score),
         favored_coach_beat_spread_flag = case_when(favored_result < favored_spread ~ 1, TRUE ~ 0),
         favored_coach_beat_spread_margin = favored_spread -favored_result,
         dog_coach_beat_spread_flag = case_when(favored_result > favored_spread ~ 1, TRUE ~ 0),
         dog_coach_beat_spread_margin = favored_result - favored_spread)

skim(games_new)


# Coach level -------------------------------------------------------------
favored_coaches <- games_new %>%
  filter(season < 2020) %>%
  group_by(favored_coach) %>%
  summarise(fav_points_over_expected = sum(favored_coach_beat_spread_margin),
            fav_points_over_expected_avg = mean(favored_coach_beat_spread_margin),
            average_fav_spread = mean(-spread_line),
            average_fav_margin = mean(-result),
            fav_beat_spread = sum(favored_coach_beat_spread_flag),
            fav_beat_spread_avg = fav_beat_spread / n(),
            fav_games = n()) %>%
  ungroup()

dog_coaches <- games_new %>%
  filter(season < 2020) %>%
  group_by(dog_coach) %>%
  summarise(dog_points_over_expected = sum(dog_coach_beat_spread_margin),
            dog_points_over_expected_avg = mean(dog_coach_beat_spread_margin),
            average_dog_spread = mean(spread_line),
            average_dog_margin = mean(result),
            dog_beat_spread = sum(dog_coach_beat_spread_flag),
            dog_beat_spread_avg = dog_beat_spread / n(),  
            dog_games = n()) %>%
  ungroup()

coaches <- favored_coaches %>%
  full_join(dog_coaches, by = c("favored_coach"="dog_coach")) %>%
  mutate(total_games = fav_games + dog_games,
         total_points_over_expected = fav_points_over_expected + dog_points_over_expected,
         points_over_expected_avg = total_points_over_expected/total_games,
         beat_spread_avg = (fav_beat_spread + dog_beat_spread)/total_games)

coaches %>%
  filter(total_games > 80) %>%
  ggplot(aes(x = fav_points_over_expected_avg, y = dog_points_over_expected_avg, label = favored_coach)) + 
  geom_point() +
  geom_text(position = "dodge", vjust = "inward", hjust = "inward")

coaches %>%
  filter(total_games > 75) %>%
  ggplot(aes(x = points_over_expected_avg, y = beat_spread_avg, label = favored_coach)) + 
  geom_point() +
  geom_text(position = "dodge", vjust = "inward", hjust = "inward")


# Year-over-year testing --------------------------------------------------
favored_coaches_seasons <- games_new %>%
  filter(season < 2020) %>%
  group_by(favored_coach, season) %>%
  summarise(fav_points_over_expected = sum(favored_coach_beat_spread_margin),
            fav_points_over_expected_avg = mean(favored_coach_beat_spread_margin),
            average_fav_spread = mean(-spread_line),
            average_fav_margin = mean(-result),
            fav_beat_spread = sum(favored_coach_beat_spread_flag),
            fav_beat_spread_avg = fav_beat_spread / n(),
            fav_games = n()) %>%
  ungroup() %>%
  arrange(favored_coach, season) %>%
  group_by(favored_coach) %>%
  mutate(lag_fav_points_over_expected_avg = lag(fav_points_over_expected_avg),
         lag_fav_beat_spread_avg = lag(fav_beat_spread_avg)) %>%
  ungroup()

dog_coaches_seasons <- games_new %>%
  filter(season < 2020) %>%
  group_by(dog_coach, season) %>%
  summarise(dog_points_over_expected = sum(dog_coach_beat_spread_margin),
            dog_points_over_expected_avg = mean(dog_coach_beat_spread_margin),
            average_dog_spread = mean(spread_line),
            average_dog_margin = mean(result),
            dog_beat_spread = sum(dog_coach_beat_spread_flag),
            dog_beat_spread_avg = dog_beat_spread / n(),  
            dog_games = n()) %>%
  ungroup() %>%
  arrange(dog_coach, season) %>%
  group_by(dog_coach) %>%
  mutate(lag_dog_points_over_expected_avg = lag(dog_points_over_expected_avg),
         lag_dog_beat_spread_avg = lag(dog_beat_spread_avg)) %>%
  ungroup()

coaches_seasons <- favored_coaches_seasons %>%
  full_join(dog_coaches_seasons, by = c("favored_coach"="dog_coach", "season")) %>%
  mutate(total_games = fav_games + dog_games,
         total_points_over_expected = fav_points_over_expected + dog_points_over_expected,
         points_over_expected_avg = total_points_over_expected/total_games,
         beat_spread_avg = (fav_beat_spread + dog_beat_spread)/total_games) %>%
  arrange(favored_coach, season) %>%
  group_by(favored_coach) %>%
  mutate(lag_points_over_expected_avg = lag(points_over_expected_avg),
         lag_beat_spread_avg = lag(beat_spread_avg)) %>%
  ungroup()

coaches_seasons %>%
  ggplot(aes(x = lag_fav_points_over_expected_avg, y = fav_points_over_expected_avg)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

coaches_seasons %>%
  ggplot(aes(x = lag_dog_points_over_expected_avg, y = dog_points_over_expected_avg)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

coaches_seasons %>%
  ggplot(aes(x = lag_points_over_expected_avg, y = points_over_expected_avg)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

coaches_seasons %>%
  ggplot(aes(x = lag_fav_beat_spread_avg, y = fav_beat_spread_avg)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

coaches_seasons %>%
  ggplot(aes(x = lag_dog_beat_spread_avg, y = dog_beat_spread_avg)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

coaches_seasons %>%
  ggplot(aes(x = lag_beat_spread_avg, y = beat_spread_avg)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = formula) +
  stat_poly_eq(formula = formula, parse = TRUE)

# Career to Date testing --------------------------------------------------

games_rolling <- games_new %>%
  arrange(favored_coach, season, week) %>%
  group_by(favored_coach) %>%
  mutate(fav_beat_spread_avg_todate = slide_dbl(favored_coach_beat_spread_margin, ~mean(.x), .before = Inf, .after = -1),
         fav_beat_spread_rate_todate = slide_dbl(favored_coach_beat_spread_flag, ~mean(.x), .before = Inf, .after = -1),
         fav_spread_avg_todate = slide_dbl(favored_spread, ~mean(.x), .before = Inf, .after = -1),
         fav_margin_avg_todate = slide_dbl(favored_result, ~mean(.x), .before = Inf, .after = -1)
         ) %>%
  ungroup() %>%
  arrange(dog_coach, season, week) %>%
  group_by(dog_coach) %>%
  mutate(dog_beat_spread_avg_todate = slide_dbl(dog_coach_beat_spread_margin, ~mean(.x), .before = Inf, .after = -1),
         dog_beat_spread_rate_todate = slide_dbl(dog_coach_beat_spread_flag, ~mean(.x), .before = Inf, .after = -1),
         dog_spread_avg_todate = slide_dbl(-favored_spread, ~mean(.x), .before = Inf, .after = -1),
         dog_margin_avg_todate = slide_dbl(-favored_result, ~mean(.x), .before = Inf, .after = -1)
  ) %>%
  ungroup()

lm1 <- lm(favored_coach_beat_spread_margin ~ fav_beat_spread_avg_todate + fav_beat_spread_rate_todate + fav_spread_avg_todate + fav_margin_avg_todate +
            dog_beat_spread_avg_todate + dog_beat_spread_rate_todate + dog_spread_avg_todate + dog_margin_avg_todate +
            season + week + temp + wind + surface + weekday,
   data = games_rolling )

lm2 <- lm(favored_coach_beat_spread_flag ~ fav_beat_spread_avg_todate + fav_beat_spread_rate_todate + fav_spread_avg_todate + fav_margin_avg_todate +
            dog_beat_spread_avg_todate + dog_beat_spread_rate_todate + dog_spread_avg_todate + dog_margin_avg_todate +
            season + week + temp + wind + surface + weekday,
          data = games_rolling )

summary(lm2)

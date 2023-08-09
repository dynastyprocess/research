library(brms)
library(tidyverse)
strikeout_data <- readRDS("~/Documents/DynastyProcess/research/mlb-prop-modeling/strikeout_data.RDS")

strikeout_data %>%
  ggplot(aes(x = predicted_so, y = SO)) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  geom_abline() +
  theme_minimal()

first_model <-
  brm(data = strikeout_data,
      family = gaussian,
      # family = bernoulli(link = identity),
      formula = SO ~ predicted_so,
      # prior(beta(2, 2), class = Intercept, lb = 0, ub = 1),
      # iter = 500 + 3334, 
      # warmup = 500,
      # chains = 3,
      seed = 815)

plot(first_model)
print(first_model)
posterior_summary(first_model, robust = T)
posterior_summary(first_model, probs = c(.025, .25, .75, .975))

draws <- as_draws_df(first_model)

draws %>% 
  ggplot(aes(x = b_Intercept)) +
  geom_histogram(color = "grey92", fill = "grey67",
                 linewidth = .2) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Theta via ggplot2::geom_histogram()",
       x = expression(theta)) +
  theme_minimal() +
  theme(plot.title.position = "plot")


newdata <- tibble(predicted_so = runif(10,2,6))
library(tidybayes)
tidy_pred <- first_model %>% 
  predicted_draws(newdata = newdata)

tidy_pred |> 
  ggplot(aes(x = .prediction)) +
  geom_histogram() +
  theme_minimal() +
  facet_wrap(~predicted_so)

test <- 
  tidy_pred |> 
  group_by(.row) |> 
  select(.prediction) |> 
  skimr::skim()

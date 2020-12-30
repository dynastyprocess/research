#Unused
best1 <- show_best(xgb_res, "recall")
best2 <- show_best(xgb_res, "precision")

xgb_res %>%
  collect_metrics

xgb_res %>%
  collect_metrics %>% 
  #filter(mixture == 0.5) %>% 
  select(mixture, .metric, mean) %>% 
  ggplot(aes(mixture, mean, color = .metric)) +
  geom_point(alpha = 0.8, size = 2) +
  theme_minimal() +
  facet_wrap(~.metric, scales = "free_y")


xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "precision") %>%
  select(mean, penalty:mixture) %>%
  pivot_longer(penalty:mixture,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "precision")

best_auc <- select_best(xgb_res, "precision")

final_xgb <- finalize_workflow(
  xgb_workflow,
  best_auc
)

final_xgb %>%
  fit(data = df_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res <- last_fit(final_xgb, df_split)

collect_metrics(final_res)


df_train_fit <- fit(final_xgb, data = df_train)

df_train %>%
  bind_cols(predict(df_train_fit, df_train)) %>% 
  yardstick::precision(as.factor(parlay_td_next), .pred_class)

df_test %>%
  bind_cols(predict(df_train_fit, df_test)) %>% 
  yardstick::precision(as.factor(parlay_td_next), .pred_class)

temp2 <- df_test %>%
  bind_cols(predict(df_train_fit, df_test, type = "prob"))  %>% 
  mutate(across(where(is.numeric), round, 2)) %>%
  arrange(-.pred_1) %>% 
  select(.pred_1, .pred_0, parlay_td_next,
         implied_total_next, total_fp_x_roll2, rush_fp_roll16, rush_fp_roll8,
         rush_td_team_x_roll16, pass_att_team_roll16, rush_yd_team_x, total_yd_x, rush_yd_x_roll1)

gain_curve(temp2, as.factor(parlay_td_next), .pred_1) %>% autoplot()
lift_curve(temp2, as.factor(parlay_td_next), .pred_0) %>% autoplot()

# df_test %>%
#   bind_cols(predict(df_train_fit, df_test, type = "prob")) %>%
#   mutate(tdpred = ifelse(.pred_1>=0.5,1,0)) %>% 
#   conf_mat(as.factor(parlay_td_next), as.factor(tdpred)) %>% autoplot()

pr_curve(temp2, as.factor(parlay_td_next), .pred_0) %>% 
  ggplot(aes(x=.threshold)) +
  geom_point(aes(y=precision), color = "red") +
  geom_point(aes(y=recall), color = "black") +
  theme_minimal()

temp %>% 
  ggplot(aes(x=.threshold)) +
  geom_point(aes(y=specificity)) +
  geom_point(aes(y=sensitivity, color = "red"))

temp <- final_res %>%
  collect_predictions() %>%
  roc_curve(`as.factor(parlay_td_next)`, .pred_0) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )

temp2 %>% 
  mutate(tdpred = ifelse(.pred_1>=0.5,1,0)) %>% 
  group_by(tdpred) %>% 
  summarise(mean(.pred_1),
            mean(parlay_td_next),
            sum(parlay_td_next),
            n())

temp2 %>% 
  ggplot(aes(x=.pred_1)) +
  geom_histogram()

temp2 %>% 
  ggplot(aes(x=implied_total_next, y=pass_att_team_roll16, z = parlay_td_next_pred)) +
  stat_summary_hex() + 
  scale_color_gradient2(palette = "Greens") +
  theme_minimal() +
  geom_smooth() +
  facet_wrap("parlay_td_next")

test_rs %>%
  metrics(parlay_td_next, parlay_td_next_pred)


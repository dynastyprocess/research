

sanity_check <- 
  joined_weekly_df %>% 
  group_by(season, week, position) %>% 
  arrange(rank) %>% 
  mutate(pos_rank = row_number(rank)) %>% 
  ungroup() %>% 
  group_by(position, pos_rank) %>% 
  summarise(n(),
            over6.5_rate = mean(receptions >= 7),
            over_72.5_rate = mean(rec_yards_gained >= 73),
            meadian_rec = median(receptions),
            median_rec_yards = median(rec_yards_gained)) %>% 
  ungroup()

sanity_check %>% 
  filter(position == "WR",
         pos_rank <= 36) %>% 
  ggplot(aes(x = pos_rank,
             y = over6.5_rate)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  theme_minimal()

sanity_check %>% 
  filter(position == "WR",
         pos_rank <= 36) %>% 
  ggplot(aes(x = pos_rank,
             y = over_72.5_rate)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  theme_minimal()

# Compare to old EP -------------------------------------------------------
old_df2019 <- dfnewmerged %>%
  filter(week <= 17) %>%
  group_by(rusher_player_id) %>%
  summarise(ep_old = sum(eFP, na.rm=TRUE),
            rushep_old = sum(eRushFP, na.rm = TRUE),
            recep_odl = sum(eRecFP, na.rm = TRUE),
            fp_old = sum(FP, na.rm = TRUE),
            rushfp_old = sum(RushFP, na.rm = TRUE),
            recfp_old = sum(RecFP, na.rm = TRUE)) %>%
  ungroup()

new_df2019 <- all_games %>%
  filter(season == "2019", week <= 17) %>%
  group_by(gsis_name, player_id) %>%
  summarise(ep_new = sum(eTotalFP, na.rm = TRUE),
            rushep_new = sum(eRushFP, na.rm = TRUE),
            recep_new = sum(eRecFP, na.rm = TRUE),
            fp_new = sum(TotalFP, na.rm = TRUE),
            rushfp_new = sum(rushFP, na.rm = TRUE),
            recfp_new = sum(recFP, na.rm = TRUE)) %>%
  ungroup()

merged_df <- new_df2019 %>%
  left_join(old_df2019, by = c("player_id" = "rusher_player_id")) %>%
  mutate(rushdif = rushep_new - rushep_old,
         recdif = recep_new - recep_odl)

merged_df %>%
  rsq(rushfp_new, rushep_new)

merged_df %>%
  rsq(rushfp_new, rushep_old)


merged_df %>%
  rsq(recfp_new, recep_new)

merged_df %>%
  rsq(recfp_new, recep_odl)

merged_example <- all_games %>%
  filter(season == "2019", gsis_name == "Michael Thomas") %>%
  inner_join(dfnewmerged, by = c("player_id" = "rusher_player_id", "week")) %>%
  select(week, eRecFP.x, eRecFP.y)

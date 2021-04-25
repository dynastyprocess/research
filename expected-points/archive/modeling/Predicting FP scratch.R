


# Rolling Averages ---------------------------------------------
games_slide <- all_games %>%
  filter(substr(game_id,1,4) >= 2006) %>%
  #filter(gsis_name == "Christian Kirk") %>%
  mutate(game_id = as.factor(game_id)) %>%
  arrange(player_id, game_id) %>%
  group_by(player_id, gsis_name, gsis_pos) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(#AvgToDate = across(is.numeric, ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1)),
    #AvgPrev2 = across(is.numeric, ~slide_dbl(.x, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1, .complete = TRUE)),
    
    #Expected Points
    AvgEP_ToDate = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgEP_Prev2 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgEP_Prev4 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgEP_Prev6 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgEP_Prev8 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgEP_Prev10 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgEP_Prev12 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgEP_Prev14 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgEP_Prev16 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgEP_Prev18 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgEP_Prev20 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgEP_Prev22 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgEP_Prev24 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgEP_Prev26 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgEP_Prev28 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgEP_Prev30 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgEP_Prev32 = slide_dbl(eTotalFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    AvgRushEP_ToDate = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgRushEP_Prev2 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgRushEP_Prev4 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgRushEP_Prev6 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgRushEP_Prev8 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgRushEP_Prev10 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgRushEP_Prev12 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgRushEP_Prev14 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgRushEP_Prev16 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgRushEP_Prev18 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgRushEP_Prev20 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgRushEP_Prev22 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgRushEP_Prev24 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgRushEP_Prev26 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgRushEP_Prev28 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgRushEP_Prev30 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgRushEP_Prev32 = slide_dbl(eRushFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    AvgRecEP_ToDate = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgRecEP_Prev2 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgRecEP_Prev4 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgRecEP_Prev6 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgRecEP_Prev8 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgRecEP_Prev10 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgRecEP_Prev12 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgRecEP_Prev14 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgRecEP_Prev16 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgRecEP_Prev18 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgRecEP_Prev20 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgRecEP_Prev22 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgRecEP_Prev24 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgRecEP_Prev26 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgRecEP_Prev28 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgRecEP_Prev30 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgRecEP_Prev32 = slide_dbl(eRecFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    AvgPassEP_ToDate = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgPassEP_Prev2 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgPassEP_Prev4 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgPassEP_Prev6 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgPassEP_Prev8 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgPassEP_Prev10 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgPassEP_Prev12 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgPassEP_Prev14 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgPassEP_Prev16 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgPassEP_Prev18 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgPassEP_Prev20 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgPassEP_Prev22 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgPassEP_Prev24 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgPassEP_Prev26 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgPassEP_Prev28 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgPassEP_Prev30 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgPassEP_Prev32 = slide_dbl(ePassFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    #AirYards
    AvgAY_ToDate = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgAY_Prev2 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgAY_Prev4 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgAY_Prev6 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgAY_Prev8 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgAY_Prev10 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgAY_Prev12 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgAY_Prev14 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgAY_Prev16 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgAY_Prev18 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgAY_Prev20 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgAY_Prev22 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgAY_Prev24 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgAY_Prev26 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgAY_Prev28 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgAY_Prev30 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgAY_Prev32 = slide_dbl(AirYards, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    #Fantasy Points
    AvgFP_ToDate = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgFP_Prev2 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgFP_Prev4 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgFP_Prev6 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgFP_Prev8 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgFP_Prev10 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgFP_Prev12 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgFP_Prev14 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgFP_Prev16 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgFP_Prev18 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgFP_Prev20 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgFP_Prev22 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgFP_Prev24 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgFP_Prev26 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgFP_Prev28 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgFP_Prev30 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgFP_Prev32 = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    AvgRecFP_ToDate = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgRecFP_Prev2 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgRecFP_Prev4 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgRecFP_Prev6 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgRecFP_Prev8 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgRecFP_Prev10 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgRecFP_Prev12 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgRecFP_Prev14 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgRecFP_Prev16 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgRecFP_Prev18 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgRecFP_Prev20 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgRecFP_Prev22 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgRecFP_Prev24 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgRecFP_Prev26 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgRecFP_Prev28 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgRecFP_Prev30 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgRecFP_Prev32 = slide_dbl(recFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    AvgRushFP_ToDate = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgRushFP_Prev2 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgRushFP_Prev4 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgRushFP_Prev6 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgRushFP_Prev8 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgRushFP_Prev10 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgRushFP_Prev12 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgRushFP_Prev14 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgRushFP_Prev16 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgRushFP_Prev18 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgRushFP_Prev20 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgRushFP_Prev22 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgRushFP_Prev24 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgRushFP_Prev26 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgRushFP_Prev28 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgRushFP_Prev30 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgRushFP_Prev32 = slide_dbl(rushFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    AvgPassFP_ToDate = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = Inf, .after = -1),
    AvgPassFP_Prev2 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 2, .after = -1),
    AvgPassFP_Prev4 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 4, .after = -1),
    AvgPassFP_Prev6 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 6, .after = -1),
    AvgPassFP_Prev8 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 8, .after = -1),
    AvgPassFP_Prev10 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 10, .after = -1),
    AvgPassFP_Prev12 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 12, .after = -1),
    AvgPassFP_Prev14 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 14, .after = -1),
    AvgPassFP_Prev16 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 16, .after = -1),
    AvgPassFP_Prev18 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 18, .after = -1),
    AvgPassFP_Prev20 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 20, .after = -1),
    AvgPassFP_Prev22 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 22, .after = -1),
    AvgPassFP_Prev24 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 24, .after = -1),
    AvgPassFP_Prev26 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 26, .after = -1),
    AvgPassFP_Prev28 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 28, .after = -1),
    AvgPassFP_Prev30 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 30, .after = -1),
    AvgPassFP_Prev32 = slide_dbl(passFP, ~mean(.x, na.rm =TRUE), .before = 32, .after = -1),
    
    ROCAvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = Inf),
    Next4_AvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = 4),
    Next8_AvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = 8),
    Next12_AvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = 12),
    Next16_AvgFP = slide_dbl(TotalFP, ~mean(.x, na.rm =TRUE), .before = -1, .after = 16),
    
  ) %>%
  ungroup()



# Mars Model ---------------------------------------------
fp_model <- function(df){
  earth(Next16_AvgFP ~ ., data = df, degree = 2)
}


slide_reg <- games_slide %>%
  filter(!is.na(ROCAvgFP)) %>%
  select(Next16_AvgFP, gsis_pos, starts_with("Avg")) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(gsis_pos = as.factor(gsis_pos)) %>%
  na.omit()

sliding_split <- initial_split(slide_reg, prop = 4/5)

# formulas <- c("RocAvgFP ~ ., data = df",
#               #"Next4_AvgFP ~ ., data = df",
#               "Next8_AvgFP ~ ., data = df",
#               #"Next12_AvgFP ~ ., data = df",
#               "Next16_AvgFP ~ ., data = df")

slidedf_train <- training(sliding_split) %>%
  group_by(gsis_pos) %>%
  nest() %>%
  mutate(model = future_map(data, fp_model),
         summs = future_map(model, summary),
         vips = future_map(model, vip))


slidedf_train$gsis_pos[[3]]
slidedf_train$summs[[3]]

slidedf_train$vips[[3]]

pdp::partial(slidedf_train$model[[3]], pred.var = "AvgFP_Prev28", train = slidedf_train$data[[3]]) %>% autoplot()
pdp::partial(slidedf_train$model[[4]], pred.var = c("AvgFP_Prev14","AvgEP_Prev4"), train = slidedf_train$data[[4]]) %>% autoplot()

temp <- slidedf_train$data[[3]]
temp2 <- games_slide %>% filter(AvgAY_ToDate < 0, gsis_pos == "QB") %>% arrange(-ROCAvgFP)

qbpred <- games_slide %>%
  filter(gsis_pos == "QB")

qbpred <- qbpred %>%
  bind_cols(pred = predict(slidedf_train$model[[3]], newdata = qbpred)) %>%
  unnest(cols = c(pred)) %>%
  rename(Next16Pred = pred) %>%
  select(is.character, Next16Pred, game_id)

LatestPrediction <- qbpred %>%
  filter(substr(game_id,1,4) == 2018) %>%
  group_by(player_id) %>%
  mutate(game_id = as.character(game_id),
         maxgame_id = max(game_id)) %>%
  ungroup() %>%
  dplyr::select(player_id, maxgame_id) %>%
  distinct()

QBs2020 <- qbpred %>%
  inner_join(LatestPrediction, by = c("player_id","game_id" = "maxgame_id"))

# vars <- games_slide %>% select(contains("Pass")) %>% names()
# 
# lm(as.formula(Next16_AvgFP ~ vars), data = games_slide)
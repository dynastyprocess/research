model_obj <- .load_model_objs("rushing_yards")

model_obj$model$params$objective <- "reg"

preprocessed_pbp <- ep_preprocess(nflreadr::load_pbp(2021))

rush_df <-
  preprocessed_pbp$rush_df %>%
  hardhat::forge(new_data = ., blueprint = model_obj$blueprint)

ep_load <- rush_df$predictors %>%
  dplyr::mutate(rushing_yards = preprocessed_pbp$rush_df$rushing_yards) %>%
  as.matrix()

ep_load <- load_ep_pbp_rush(2018:2021) %>%
  as.matrix()

rush_yards_explainer <-
  DALEXtra::explain_xgboost(
    model = model_obj$model,
    data = ep_load,
    y =  ep_load[,14])



undebug(DALEXtra::explain_xgboost)

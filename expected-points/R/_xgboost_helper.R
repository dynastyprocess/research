library(xgboost)
library(hardhat)
library(nflreadr)
library(magrittr)
# pkgload::load_code()
# something finicky with having it in data/, moved to inst for now
fit_rush_yards <- readRDS("models_xgboost/fit_rush_yards.RDS")

fit_rush_yards$fit$fit$fit %>%
  xgboost::xgb.Booster.complete() %>%
  xgboost::xgb.save("models_prod/rush_yard.xgb")

model_rush_yards <- xgboost::xgb.load("models_prod/rush_yard.xgb")

fit_pass_comp_blueprint <- fit_rush_yards$pre$mold$blueprint

save(fit_pass_comp_blueprint, file = "models_prod/rush_yard.bpt")

# preprocess <- ep_preprocess(nflreadr::load_pbp())
# forged <- hardhat::forge(preprocess$pass_df,fit_pass_comp_blueprint)
# 
# predict(model_pass_completion, forged$predictors %>%  as.matrix())

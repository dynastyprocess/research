library(xgboost)
library(hardhat)
library(nflreadr)
library(magrittr)
# pkgload::load_code()
# something finicky with having it in data/, moved to inst for now
fit_pass_fd <- readRDS("models_xgboost/fit_pass_fd.RDS")

fit_pass_fd$fit$fit$fit %>%
  xgboost::xgb.Booster.complete() %>%
  xgboost::xgb.save("models_prod/pass_fd.xgb")

model_pass_fd <- xgboost::xgb.load("models_prod/pass_fd.xgb")

fit_pass_fd_blueprint <- fit_pass_fd$pre$mold$blueprint

saveRDS(fit_pass_fd_blueprint, file = "models_prod/pass_fd.rds")

# preprocess <- ep_preprocess(nflreadr::load_pbp())
# forged <- hardhat::forge(preprocess$pass_df,fit_pass_comp_blueprint)
# 
# predict(model_pass_fd, forged$predictors %>%  as.matrix())

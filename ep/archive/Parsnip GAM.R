library(parsnip)
library(mgcv)
library(tidyverse)

str(mgcv::bam)

set_new_model("mgcv_bam")
set_model_mode(model = "mgcv_bam", mode = "regression")
set_model_engine(
  "mgcv_bam", 
  mode = "regression", 
  eng = "mgcv"
)

set_model_arg(
  model = "mgcv_bam",
  eng = "mgcv",
  parsnip = "method",
  original = "method",
  func = list(pkg = "mgcv", fun = "method"),
  has_submodel = FALSE
)

set_fit(
  model = "mgcv_bam",
  eng = "mgcv",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "mgcv", fun = "bam"),
    defaults = list()
  )
)

class_info <- 
  list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      # These lists should be of the form:
      # {predict.mda argument name} = {values provided from parsnip objects}
      list(
        # We don't want the first two arguments evaluated right now
        # since they don't exist yet. `type` is a simple object that
        # doesn't need to have its evaluation deferred. 
        object = expr(object$fit),
        newdata = expr(new_data),
        type = "numeric"
      )
  )

set_pred(
  model = "mgcv_bam",
  eng = "mgcv",
  mode = "regression",
  type = "numeric",
  value = class_info
)

mgcv_bam <-
  function(mode = "regression",  method = NULL) {
    # Check for correct mode
    if (mode  != "regression") {
      stop("`mode` should be 'regression'", call. = FALSE)
    }
    
    # Capture the arguments in quosures
    args <- list(method = rlang::enquo(method))
    
    # Save some empty slots for future parts of the specification
    out <- list(args = args, eng_args = NULL,
                mode = mode, method = NULL, engine = NULL)
    
    # set classes in the correct order
    class(out) <- make_classes("mgcv_bam")
    out
  }

mgcv_bam(method = "REML") %>%
  translate(engine = "mgcv")

show_model_info("mgcv_bam")

library(rsample)
library(tibble)

set.seed(4622)
iris_split <- initial_split(iris, prop = 0.95)
iris_train <- training(iris_split)
iris_test  <-  testing(iris_split)

mda_spec <- mgcv_bam(method = "GCV.Cp") %>% 
  set_engine("mgcv")

mda_fit <- mda_spec %>%
  fit(Petal.Width ~ s(Sepal.Width), data = iris_train, engine = "mgcv")
predict(mda_fit, iris_test)

library(tidymodels)
iris_test$price_pred <- predict(mda_fit, iris_test)
iris_test$price_pred

rmse(iris_test$price_pred, truth = iris_test$Petal.Width, estimate = .pred)
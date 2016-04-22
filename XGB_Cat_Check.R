checkcat <- function(xcol) {
  train_test <- train_test[ ,xcol]
  train_watch <- train_watch[ ,xcol]
  xgtrain <-  xgb.DMatrix(as.matrix(as.numeric(train_test)), label = label_train_test, missing=NA)
  xgtrain_watch <-  xgb.DMatrix(as.matrix(as.numeric(train_watch)), label = label_train_watch, missing=NA)
  
  param0 <- list(
    # some generic, non specific params
    "objective"  = "binary:logistic",
    "eval_metric" = "auc",
    "eta" = 0.1,
    "subsample" = 0.9,
    "min_child_weight" = 1,
    "max_depth" = 2
  )
  model_cv <- xgb.cv(
    params = param0,
    nrounds = 500,
    nfold = 5,
    data = xgtrain,
    early.stop.round = 3,
    maximize = TRUE,
    verbose = TRUE
  )

  model.xgb <-  xgb.train(
    nrounds = 500,
    params = param0,
    data = xgtrain,
    watchlist = list(val=xgtrain_watch),
    verbose = 1,
    early.stop.round = 5,
  )
  cat("\n\nImpact Categorical AUC \n")
  train_test <- as.data.frame(train_test)
  train_watch <- as.data.frame(train_watch)
  colnames(train_test) <- c(xcol)
  colnames(train_watch) <- c(xcol)
  train_test$impact <- catimpact(train_test[[xcol]], label_train_test)
  impact <- unique(train_test)
  train_test[ , 1] <- NULL
  train_watch <- left_join(train_watch, impact, by=xcol)
  train_watch[ , 1] <- NULL
  train_test <- as.matrix(train_test)
  train_watch <- as.matrix(train_watch)
  xgtrain <-  xgb.DMatrix(as.matrix(as.numeric(train_test)), label = label_train_test, missing=NA)
  xgtrain_watch <-  xgb.DMatrix(as.matrix(as.numeric(train_watch)), label = label_train_watch, missing=NA)
  
  param0 <- list(
    # some generic, non specific params
    "objective"  = "binary:logistic",
    "eval_metric" = "auc",
    "eta" = 0.1,
    "subsample" = 0.9,
    "min_child_weight" = 1,
    "max_depth" = 2
  )
  model_cv <- xgb.cv(
    params = param0,
    nrounds = 500,
    nfold = 5,
    data = xgtrain,
    early.stop.round = 3,
    maximize = TRUE,
    verbose = TRUE
  )
  
  model.xgb <-  xgb.train(
    nrounds = 500,
    params = param0,
    data = xgtrain,
    watchlist = list(val=xgtrain_watch),
    verbose = 1,
    early.stop.round = 5,
  )
}

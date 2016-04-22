setwd("C:/Users/Mavis Xue Home/Desktop/Bigdata/SCS")
require(xgboost)
require(data.table)
require(dplyr)
require(caret)
rm(list = ls())
gc()
train <- as.data.frame(fread("train.csv"))

train$ID <- NULL
#Split
trainIndex <- createDataPartition(train$TARGET, p = .8,
                                  list = FALSE,
                                  times = 1)

train_test <- train[ trainIndex,]
train_watch  <- train[-trainIndex,]

label_train_test <- train_test$TARGET
label_train_watch <- train_watch$TARGET
train_test$TARGET <- train_watch$TARGET <- NULL

##### Extracting TARGET
feature.names <- c('var38','var15')


train_test <- train_test[, feature.names]
train_watch <- train_watch[, feature.names]

#Impact
catimpact <- function(xcol, targetcol) {
    x_impact <- rep(0, length(xcol))
    for (i in unique(xcol)) {
        x_impact[xcol == i] <- mean(targetcol[xcol == i]) 
    }
    return(x_impact)
}
train_test$var38 <- round(train_test$var38/10, 0)
train_test$var38_impact <- catimpact(train_test$var38, label_train_test)
train_test$var15_impact <- catimpact(train_test$var15, label_train_test)
var38 <- unique(select(train_test, var38, var38_impact))
var15 <- unique(select(train_test, var15, var15_impact))

train_test$var38 <- train_test$var15 <- NULL
train_watch$var38 <- round(train_watch$var38/10, 0)
train_watch <- left_join(train_watch, var38, by = 'var38') %>%
            left_join(var15, by = 'var15')
train_watch$var38 <- train_watch$var15 <- NULL


xgtrain <-  xgb.DMatrix(as.matrix(train_test), label = label_train_test, missing=NA)
xgtrain_watch <-  xgb.DMatrix(as.matrix(train_watch), label = label_train_watch, missing=NA)

# Do cross-validation with xgboost - xgb.cv
param0 <- list(
    # some generic, non specific params
    "objective"  = "binary:logistic",
    "eval_metric" = "auc",
    "eta" = 0.02,
    "subsample" = 0.9,
    "colsample_bytree" = 0.7,
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

best <- max(model_cv$test.auc.mean)
bestIter <- which(model_cv$test.auc.mean==best)    

model.xgb <-  xgb.train(
    nrounds = 500,
    params = param0,
    data = xgtrain,
    watchlist = list(val=xgtrain_watch),
    verbose = 1,
    early.stop.round = 5,
)

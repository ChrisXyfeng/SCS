library(Matrix)
lable_train <- train$TARGET
lable_train <- as.factor(lable_train)

train <- sparse.model.matrix(TARGET ~ ., data = train)

test_sparse <-  sparse.model.matrix(~.,test)

library(caret)
fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 2,
    ## repeated ten times
    repeats = 2)

LogitBoost <- train(x=train_sparse, y=lable_train,data = training,
                               method = "LMT",
                               trControl = fitControl,
                               ## This last option is actually one
                               ## for gbm() that passes through
                               verbose = TRUE)

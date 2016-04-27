require(caret)
require(data.table)
require(Matrix)
rm(list = ls())
gc(reset = TRUE)

######################################Split Data###########################################
# train <- as.data.frame(fread("train_pre.csv", integer64 = 'numeric'))
# set.seed(130622)
# trainIndex <- createDataPartition(train$TARGET, p = .8,
#                                   list = FALSE,
#                                   times = 1)
# train <- train[ trainIndex,]
# cal  <- train[-trainIndex,]
# write.csv(train, "train_split.csv", row.names = FALSE)
# write.csv(cal, "cal_split.csv", row.names = FALSE)

##################################Let's Model !############################################
train <- as.data.frame(fread("train_split.csv", integer64 = 'numeric'))
cal <- as.data.frame(fread("cal_split.csv", integer64 = 'numeric'))
test <- as.data.frame(fread("test_pre.csv", integer64 = 'numeric'))
label_train <- as.factor(train$TARGET)
label_cal <- as.factor(cal$TARGET)
train$TARGET <- cal$TARGET <- NULL

#Sparse Matrix
# train <- sparse.model.matrix(~., data = train)
# cal <- sparse.model.matrix(~., data = cal)
#Matrix
train <- as.matrix(train)
cal <- as.matrix(cal)
test <- as.matrix(test)

#Models in Caret
control_clf <- trainControl(method = "cv",
                            number = 4,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
tune_xgb_1 <- data.frame(nrounds = 560,
                         max_depth = 5,
                         eta = 0.0202048,
                         gamma = 0,
                         colsample_bytree = 0.701,
                         min_child_weight = 1)

set.seed(130622)
clf_xgb_1 <- train(x=train, y=label_train,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = control_clf,
                   verbose = TRUE,
                   tuneGrid = tune_xgb_1)
clf_xgb_1
meta_train_xgb_1 <- clf_xgb_1$pred
meta_train_xgb_1 <- meta_train_xgb_1[order(meta_train_xgb_1$rowIndex),]

#Predictive on the Cal and Test
meta_cal_xgb_1 <- predict.train(clf_xgb_1, cal, type = "prob")
meta_test_xgb_1 <- predict(clf_xgb_1, test, type = "prob")

write.csv(meta_train_xgb_1, "meta_train_xgb_1.csv", row.names = FALSE)
write.csv(meta_cal_xgb_1, "meta_cal_xgb_1.csv", row.names = FALSE)
write.csv(meta_test_xgb_1, "meta_test_xgb_1.csv", row.names = FALSE)

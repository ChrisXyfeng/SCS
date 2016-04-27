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
levels(label_train) <- c("Neg", "Pos")
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
############################################################################################################
###########XGB_1
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

###########XGB_2
tune_xgb_2 <- data.frame(nrounds = 560,
                         max_depth = 2,
                         eta = 0.0202048,
                         gamma = 0,
                         colsample_bytree = 0.701,
                         min_child_weight = 1)

set.seed(130622)
clf_xgb_2 <- train(x=train, y=label_train,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = control_clf,
                   verbose = TRUE,
                   tuneGrid = tune_xgb_2)
clf_xgb_2
meta_train_xgb_2 <- clf_xgb_2$pred
meta_train_xgb_2 <- meta_train_xgb_2[order(meta_train_xgb_2$rowIndex),]

#Predictive on the Cal and Test
meta_cal_xgb_2 <- predict.train(clf_xgb_2, cal, type = "prob")
meta_test_xgb_2 <- predict(clf_xgb_2, test, type = "prob")

write.csv(meta_train_xgb_2, "meta_train_xgb_2.csv", row.names = FALSE)
write.csv(meta_cal_xgb_2, "meta_cal_xgb_2.csv", row.names = FALSE)
write.csv(meta_test_xgb_2, "meta_test_xgb_2.csv", row.names = FALSE)

###########XGB_3
tune_xgb_3 <- data.frame(nrounds = 560,
                         max_depth = 8,
                         eta = 0.0202048,
                         gamma = 0,
                         colsample_bytree = 0.701,
                         min_child_weight = 1)

set.seed(130622)
clf_xgb_3 <- train(x=train, y=label_train,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = control_clf,
                   verbose = TRUE,
                   tuneGrid = tune_xgb_3)
clf_xgb_3
meta_train_xgb_3 <- clf_xgb_3$pred
meta_train_xgb_3 <- meta_train_xgb_3[order(meta_train_xgb_3$rowIndex),]

#Predictive on the Cal and Test
meta_cal_xgb_3 <- predict.train(clf_xgb_3, cal, type = "prob")
meta_test_xgb_3 <- predict(clf_xgb_3, test, type = "prob")

write.csv(meta_train_xgb_3, "meta_train_xgb_3.csv", row.names = FALSE)
write.csv(meta_cal_xgb_3, "meta_cal_xgb_3.csv", row.names = FALSE)
write.csv(meta_test_xgb_3, "meta_test_xgb_3.csv", row.names = FALSE)

###########XGB_4
tune_xgb_4 <- data.frame(nrounds = 560,
                         max_depth = 5,
                         eta = 0.0202048,
                         gamma = 0,
                         colsample_bytree = 1,
                         min_child_weight = 1)

set.seed(130622)
clf_xgb_4 <- train(x=train, y=label_train,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = control_clf,
                   verbose = TRUE,
                   tuneGrid = tune_xgb_4)
clf_xgb_4
meta_train_xgb_4 <- clf_xgb_4$pred
meta_train_xgb_4 <- meta_train_xgb_4[order(meta_train_xgb_4$rowIndex),]

#Predictive on the Cal and Test
meta_cal_xgb_4 <- predict.train(clf_xgb_4, cal, type = "prob")
meta_test_xgb_4 <- predict(clf_xgb_4, test, type = "prob")

write.csv(meta_train_xgb_4, "meta_train_xgb_4.csv", row.names = FALSE)
write.csv(meta_cal_xgb_4, "meta_cal_xgb_4.csv", row.names = FALSE)
write.csv(meta_test_xgb_4, "meta_test_xgb_4.csv", row.names = FALSE)

###########xgb_5
tune_xgb_5 <- data.frame(nrounds = 560,
                         max_depth = 2,
                         eta = 0.0202048,
                         gamma = 0,
                         colsample_bytree = 1,
                         min_child_weight = 1)

set.seed(130622)
clf_xgb_5 <- train(x=train, y=label_train,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = control_clf,
                   verbose = TRUE,
                   tuneGrid = tune_xgb_5)
clf_xgb_5
meta_train_xgb_5 <- clf_xgb_5$pred
meta_train_xgb_5 <- meta_train_xgb_5[order(meta_train_xgb_5$rowIndex),]

#Predictive on the Cal and Test
meta_cal_xgb_5 <- predict.train(clf_xgb_5, cal, type = "prob")
meta_test_xgb_5 <- predict(clf_xgb_5, test, type = "prob")

write.csv(meta_train_xgb_5, "meta_train_xgb_5.csv", row.names = FALSE)
write.csv(meta_cal_xgb_5, "meta_cal_xgb_5.csv", row.names = FALSE)
write.csv(meta_test_xgb_5, "meta_test_xgb_5.csv", row.names = FALSE)

###########RF_1
tune_rf_1 <- data.frame(mtry = 18)

set.seed(130622)
clf_rf_1 <- train(x=train, y=label_train,
                  method = "rf",
                  metric = "ROC",
                  trControl = control_clf,
                  verbose = TRUE,
                  tuneGrid = tune_rf_1)
clf_rf_1
meta_train_rf_1 <- clf_rf_1$pred
meta_train_rf_1 <- meta_train_rf_1[order(meta_train_rf_1$rowIndex),]

#Predictive on the Cal and Test
meta_cal_rf_1 <- predict.train(clf_rf_1, cal, type = "prob")
meta_test_rf_1 <- predict(clf_rf_1, test, type = "prob")

write.csv(meta_train_rf_1, "meta_train_rf_1.csv", row.names = FALSE)
write.csv(meta_cal_rf_1, "meta_cal_rf_1.csv", row.names = FALSE)
write.csv(meta_test_rf_1, "meta_test_rf_1.csv", row.names = FALSE)

###########Boosted_Classification__Tree_1
tune_ada_1 <- data.frame(iter = 500,
                         maxdepth = 5,
                         nu = 0.02)

set.seed(130622)
clf_ada_1 <- train(x=train, y=label_train,
                   method = "ada",
                   metric = "ROC",
                   trControl = control_clf,
                   verbose = TRUE,
                   tuneGrid = tune_ada_1)
clf_ada_1
meta_train_ada_1 <- clf_ada_1$pred
meta_train_ada_1 <- meta_train_ada_1[order(meta_train_ada_1$rowIndex),]

#Predictive on the Cal and Test
meta_cal_ada_1 <- predict.train(clf_ada_1, cal, type = "prob")
meta_test_ada_1 <- predict(clf_ada_1, test, type = "prob")

write.csv(meta_train_ada_1, "meta_train_ada_1.csv", row.names = FALSE)
write.csv(meta_cal_ada_1, "meta_cal_ada_1.csv", row.names = FALSE)
write.csv(meta_test_ada_1, "meta_test_ada_1.csv", row.names = FALSE)

###########C5.0_1
tune_C5.0_1 <- data.frame(trials = 1,
                          model =  "tree",
                          winnow = TRUE)

set.seed(130622)
clf_C5.0_1 <- train(x=train, y=label_train,
                    method = "C5.0",
                    metric = "ROC",
                    trControl = control_clf,
                    verbose = TRUE,
                    tuneGrid = tune_C5.0_1)
clf_C5.0_1
meta_train_C5.0_1 <- clf_C5.0_1$pred
meta_train_C5.0_1 <- meta_train_C5.0_1[order(meta_train_C5.0_1$rowIndex),]

#Predictive on the Cal and Test
meta_cal_C5.0_1 <- predict.train(clf_C5.0_1, cal, type = "prob")
meta_test_C5.0_1 <- predict(clf_C5.0_1, test, type = "prob")

write.csv(meta_train_C5.0_1, "meta_train_C5.0_1.csv", row.names = FALSE)
write.csv(meta_cal_C5.0_1, "meta_cal_C5.0_1.csv", row.names = FALSE)
write.csv(meta_test_C5.0_1, "meta_test_C5.0_1.csv", row.names = FALSE)

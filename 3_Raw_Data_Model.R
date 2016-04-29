
require(caret)
require(data.table)
require(Matrix)
rm(list = ls())
gc(reset = TRUE)

##################################Let's Model !############################################
train <- as.data.frame(fread("train_pre.csv", integer64 = 'numeric'))
test <- as.data.frame(fread("test_pre.csv", integer64 = 'numeric'))
label_train <- as.factor(train$TARGET)
levels(label_train) <- c("Neg", "Pos")
train$TARGET <- NULL

#Sparse Matrix
# train <- sparse.model.matrix(~., data = train)
# cal <- sparse.model.matrix(~., data = cal)
#Matrix
train <- as.matrix(train)
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
meta_test_xgb_1 <- predict(clf_xgb_1, test, type = "prob")

write.csv(meta_train_xgb_1, "meta_train_xgb_1.csv", row.names = FALSE)
write.csv(meta_test_xgb_1, "meta_test_xgb_1.csv", row.names = FALSE)

###########xgb_2
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
meta_test_xgb_2 <- predict(clf_xgb_2, test, type = "prob")

write.csv(meta_train_xgb_2, "meta_train_xgb_2.csv", row.names = FALSE)
write.csv(meta_test_xgb_2, "meta_test_xgb_2.csv", row.names = FALSE)

###########xgb_3
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
meta_test_xgb_3 <- predict(clf_xgb_3, test, type = "prob")

write.csv(meta_train_xgb_3, "meta_train_xgb_3.csv", row.names = FALSE)
write.csv(meta_test_xgb_3, "meta_test_xgb_3.csv", row.names = FALSE)

###########xgb_4
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
meta_test_xgb_4 <- predict(clf_xgb_4, test, type = "prob")

write.csv(meta_train_xgb_4, "meta_train_xgb_4.csv", row.names = FALSE)
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
meta_test_xgb_5 <- predict(clf_xgb_5, test, type = "prob")

write.csv(meta_train_xgb_5, "meta_train_xgb_5.csv", row.names = FALSE)
write.csv(meta_test_xgb_5, "meta_test_xgb_5.csv", row.names = FALSE)

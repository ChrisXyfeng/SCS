require(caret)
require(data.table)
require(Matrix)
rm(list = ls())
gc(reset = TRUE)

train <- as.data.frame(fread("train.csv", integer64 = 'numeric'))
meta_train_xgb_1 <- as.data.frame(fread("meta_train_xgb_1.csv", integer64 = 'numeric'))
meta_train_xgb_2 <- as.data.frame(fread("meta_train_xgb_2.csv", integer64 = 'numeric'))
meta_train_xgb_3 <- as.data.frame(fread("meta_train_xgb_3.csv", integer64 = 'numeric'))
meta_train_xgb_4 <- as.data.frame(fread("meta_train_xgb_4.csv", integer64 = 'numeric'))
meta_train_xgb_5 <- as.data.frame(fread("meta_train_xgb_5.csv", integer64 = 'numeric'))

meta_train_xgb <- data.frame(train$TARGET, 
                             meta_train_xgb_1$Pos,
                             meta_train_xgb_2$Pos,
                             meta_train_xgb_3$Pos,
                             meta_train_xgb_4$Pos,
                             meta_train_xgb_5$Pos)
colnames(meta_train_xgb) <- c('TARGET',
                              'xgb_1',
                              'xgb_2',
                              'xgb_3',
                              'xgb_4',
                              'xgb_5')
#write.csv(meta_train_xgb, 'meta_train_xgb.csv', row.names = FALSE)

meta_test_xgb_1 <- as.data.frame(fread("meta_test_xgb_1.csv", integer64 = 'numeric'))
meta_test_xgb_2 <- as.data.frame(fread("meta_test_xgb_2.csv", integer64 = 'numeric'))
meta_test_xgb_3 <- as.data.frame(fread("meta_test_xgb_3.csv", integer64 = 'numeric'))
meta_test_xgb_4 <- as.data.frame(fread("meta_test_xgb_4.csv", integer64 = 'numeric'))
meta_test_xgb_5 <- as.data.frame(fread("meta_test_xgb_5.csv", integer64 = 'numeric'))

meta_test_xgb <- data.frame( meta_test_xgb_1$Pos,
                             meta_test_xgb_2$Pos,
                             meta_test_xgb_3$Pos,
                             meta_test_xgb_4$Pos,
                             meta_test_xgb_5$Pos)
colnames(meta_test_xgb) <- c('xgb_1',
                             'xgb_2',
                             'xgb_3',
                             'xgb_4',
                             'xgb_5')
################2-nd Level CLF as Logistic Regression################
clf_logist <- glm(TARGET ~.,family=binomial(link='logit'),data=meta_train_xgb)
pred_logist <- predict(clf_logist,newdata=meta_test_xgb,
                       type='response')
submission <- read.csv("sample.csv")
submission$TARGET <- pred_logist
write.csv(submission, "meta_xgb_logistic.csv", row.names = FALSE)

################2-nd Level CLF as XGBOOST################
meta_train_xgb$TARGET <- as.factor(meta_train_xgb$TARGET)
levels(meta_train_xgb$TARGET) <- c("Neg", "Pos")

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
clf_xgb_1 <- train(TARGET~., data = meta_train_xgb,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = control_clf,
                   verbose = TRUE,
                   tuneGrid = tune_xgb_1)

pred_xgb <- predict(clf_xgb_1, meta_test_xgb, type = "prob")$Pos
submission$TARGET <- pred_xgb
write.csv(submission, "meta_xgb_xgb.csv", row.names = FALSE)

################2-nd Level CLF as MEAN ################
pred_mean <- (meta_test_xgb$xgb_1+meta_test_xgb$xgb_2+meta_test_xgb$xgb_3+
    meta_test_xgb$xgb_4+meta_test_xgb$xgb_5)/5
submission$TARGET <- pred_mean
write.csv(submission, "meta_xgb_mean.csv", row.names = FALSE)

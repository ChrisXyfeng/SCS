setwd("C:/Users/Mavis & Xyfeng's/Desktop/Duree/SCS")
require(caret)
require(data.table)
require(Matrix)
rm(list = ls())
gc(reset = TRUE)

train_split <- as.data.frame(fread("train_split.csv", integer64 = 'numeric'))
meta_train_xgb_1 <- as.data.frame(fread("meta_train_xgb_1.csv", integer64 = 'numeric'))
meta_train_xgb_2 <- as.data.frame(fread("meta_train_xgb_2.csv", integer64 = 'numeric'))
meta_train_xgb_3 <- as.data.frame(fread("meta_train_xgb_3.csv", integer64 = 'numeric'))
meta_train_xgb_4 <- as.data.frame(fread("meta_train_xgb_4.csv", integer64 = 'numeric'))
meta_train_xgb_5 <- as.data.frame(fread("meta_train_xgb_5.csv", integer64 = 'numeric'))

meta_train_xgb <- data.frame(train_split$TARGET, 
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
write.csv(meta_train_xgb, 'meta_train_xgb.csv', row.names = FALSE)

cal_split <- as.data.frame(fread("cal_split.csv", integer64 = 'numeric'))
meta_cal_xgb_1 <- as.data.frame(fread("meta_cal_xgb_1.csv", integer64 = 'numeric'))
meta_cal_xgb_2 <- as.data.frame(fread("meta_cal_xgb_2.csv", integer64 = 'numeric'))
meta_cal_xgb_3 <- as.data.frame(fread("meta_cal_xgb_3.csv", integer64 = 'numeric'))
meta_cal_xgb_4 <- as.data.frame(fread("meta_cal_xgb_4.csv", integer64 = 'numeric'))
meta_cal_xgb_5 <- as.data.frame(fread("meta_cal_xgb_5.csv", integer64 = 'numeric'))

meta_cal_xgb <- data.frame(cal_split$TARGET, 
                           meta_cal_xgb_1$Pos,
                           meta_cal_xgb_2$Pos,
                           meta_cal_xgb_3$Pos,
                           meta_cal_xgb_4$Pos,
                           meta_cal_xgb_5$Pos)
colnames(meta_cal_xgb) <- c('TARGET',
                            'xgb_1',
                            'xgb_2',
                            'xgb_3',
                            'xgb_4',
                            'xgb_5')
write.csv(meta_cal_xgb, 'meta_cal_xgb.csv', row.names = FALSE)

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
write.csv(meta_test_xgb, 'meta_test_xgb.csv', row.names = FALSE)


meta_train_xgb <- as.data.frame(fread("meta_train_xgb.csv", integer64 = 'numeric'))
meta_cal_xgb <- as.data.frame(fread("meta_cal_xgb.csv", integer64 = 'numeric'))

label_meta_train_xgb <- as.factor(meta_train_xgb$TARGET)
levels(label_meta_train_xgb) <- c("Neg", "Pos")
label_meta_cal_xgb <- meta_cal_xgb$TARGET
meta_train_xgb$TARGET <- meta_cal_xgb$TARGET <- NULL
#Matrix
meta_train_xgb <- as.matrix(meta_train_xgb)
meta_cal_xgb <- as.matrix(meta_cal_xgb)
# test <- as.matrix(test)

#Models in Caret
control_clf <- trainControl(method = "cv",
                            number = 4,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
############################################################################################################
###########Meta_XGB_1
tune_meta_xgb_1 <- data.frame(nrounds = 560,
                         max_depth = 1,
                         eta = 0.02,
                         gamma = 0,
                         colsample_bytree = 0.701,
                         min_child_weight = 1)

set.seed(130622)
clf_meta_xgb_1 <- train(x=meta_train_xgb, y=label_meta_train_xgb,
                   method = "xgbTree",
                   metric = "ROC",
                   trControl = control_clf,
                   verbose = TRUE,
                   tuneGrid = tune_meta_xgb_1)
clf_meta_xgb_1

pred_meta_cal_xgb <- predict.train(clf_meta_xgb_1, meta_cal_xgb, type = "prob")
calcAUC(pred_meta_cal_xgb$Pos, label_meta_cal_xgb)

library(ROCR)
calcAUC <- function(predcol,outcol) {
    perf <- performance(prediction(predcol,outcol==1),'auc')
    as.numeric(perf@y.values)
}

pred_meta_test_xgb <- predict.train(clf_meta_xgb_1, meta_test_xgb, type = "prob")
submisson <- read.csv("sample_submission.csv")
submisson$TARGET <- pred_meta_test_xgb$Pos
write.csv(submisson, "blending3.csv", row.names = FALSE)

control_clf <- trainControl(method = "cv",
                            number = 4,
                            savePredictions = TRUE,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
set.seed(130622)
clf_meta_xgb_1 <- train(x=meta_train_xgb, y=label_meta_train_xgb,
                        method = "knn",
                        metric = "ROC",
                        trControl = control_clf)
clf_meta_xgb_1

pred_meta_cal_xgb <- predict.train(clf_meta_xgb_1, meta_cal_xgb, type = "prob")
calcAUC(pred_meta_cal_xgb$Pos, label_meta_cal_xgb)

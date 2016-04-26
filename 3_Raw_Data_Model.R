require(caret)
require(data.table)
require(Matrix)
rm(list = ls())
gc(reset = TRUE)
#Models on raw data
train <- as.data.frame(fread("train_pre.csv", integer64 = 'numeric'))
test <- as.data.frame(fread("test_pre.csv", integer64 = 'numeric'))
#Split data
trainIndex <- createDataPartition(train$TARGET, p = .8,
                                  list = FALSE,
                                  times = 1)
train <- train[ trainIndex,]
cal  <- train[-trainIndex,]
#Sparse Matrix
label_train <- as.factor(train$TARGET)
label_cal <- as.factor(cal$TARGET)
train$TARGET <- cal$TARGET <- NULL
#Matrix
#train <- as.matrix(~., data = train)
#cal <- as.matrix(~., data = cal)

train <- as.matrix(train)
cal <- as.matrix(cal)
#Models in Caret
control_clf <- trainControl(method = "cv",
                            number = 5,
                            savePredictions = "final")
grid_rf_1 <- expand.grid(mtry=1)
set.seed(130622)
clf_rf_1 <- train(x=train, y=label_train,
                 method = "rf",
                 trControl = control_clf,
                 verbose = FALSE,
                 tunegrid = grid_rf_1)



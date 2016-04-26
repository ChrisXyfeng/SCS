library(Matrix)
library(caret)
train <- read.csv("train_pre.csv")
test <- read.csv("test_pre.csv")

lable_train <- train$TARGET
lable_train <- as.factor(lable_train)
#Split the train data



#Sparse Matrix of the DF
train <- sparse.model.matrix(TARGET ~ ., data = train)

fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 5,
    ## repeated ten times
    repeats = 10)

LogitBoost <- train(x=train_sparse, y=lable_train,data = training,
                               method = "LMT",
                               trControl = fitControl,
                               verbose = TRUE)

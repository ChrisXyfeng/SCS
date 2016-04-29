setwd("C:/Users/Mavis Xue Home/Desktop/Bigdata/SCS")
setwd("C:/Documents and Settings/xueyu/Bureau/BD/Santander Customer Satisfaction")
require(xgboost)
require(data.table)
require(dplyr)
require(caret)
rm(list = ls())
gc()

train <- as.data.frame(fread("train.csv", integer64 = 'numeric'))
test <- as.data.frame(fread("test.csv", integer64 = 'numeric'))

##### Removing IDs
train$ID <- NULL
test$ID <- NULL


##### 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

##### Removing constant features
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

#Remove_HighCorr
train_corr <- cor(train)
high_corr <- findCorrelation(train_corr, 0.99)

# returns an index of column numbers for removal
train <- train[, -high_corr]
test <- test[, -high_corr]

write.csv(train, "train_pre.csv",row.names = FALSE)
write.csv(test, "test_pre.csv",row.names = FALSE)

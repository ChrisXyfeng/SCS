require(caret)
require(data.table)
require(Matrix)
rm(list = ls())
gc(reset = TRUE)
#Models on Cat data
train <- as.data.frame(fread("train_pre.csv", integer64 = 'numeric'))
test <- as.data.frame(fread("test_pre.csv", integer64 = 'numeric'))

#Categorical Features
vars <- read.csv('Features_Engineering.csv')
for (i in 1:307) {
    if (vars[i,'Type']=='as.factor') {
        train[,i] <- as.factor(train[,i])
        test[,i] <- as.factor(test[,i])
    }
}

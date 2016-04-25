train <- read.csv("train_pre.csv")
test <- read.csv("test_pre.csv")
#############################Feature Engineering###############################

#Calculate the unique

vars <- colnames(train)
vars <- cbind(vars,apply(train, 2, function(x) length(unique(x))))
colnames(vars) <- c('Vars','Unique')
write.csv(vars, 'Features_Engineering.csv',row.names = FALSE)

library(gmodels)
table(train$TARGET, train$saldo_medio_var17_ult1)
CrossTable(train$TARGET, train$saldo_var6, chisq = TRUE)

vars <- read.csv('Features_Engineering.csv')
for (i in 1:313) {
    if (vars[i,'Feature']=='as.facor') {
        train[,i] <- as.factor(train[,i])
        test[,i] <- as.factor(test[,i])
    }
}

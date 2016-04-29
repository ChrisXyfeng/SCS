train <- as.data.frame(fread("train_pre.csv", integer64 = 'numeric'))
test <- as.data.frame(fread("test_pre.csv", integer64 = 'numeric'))

#############################Feature Engineering###############################
###########Calculate the unique In Excel###################

vars <- colnames(train)
vars <- cbind(vars,apply(train, 2, function(x) length(unique(x))))
colnames(vars) <- c('Vars','Unique')
write.csv(vars, 'Features_Engineering.csv',row.names = FALSE)

setwd("C:/Documents and Settings/xueyu/Bureau/BD/Santander Customer Satisfaction")
require(xgboost)
require(data.table)
require(dplyr)
require(caret)
rm(list = ls())
gc()
train <- as.data.frame(fread("train_pre.csv", integer64 = 'numeric'))
test <- as.data.frame(fread("test_pre.csv", integer64 = 'numeric'))

#############################Feature Engineering###############################

###########Calculate the unique In Excel###################

vars <- colnames(train)
vars <- cbind(vars,apply(train, 2, function(x) length(unique(x))))
colnames(vars) <- c('Vars','Unique')
#write.csv(vars, 'Feature_Engineering.csv',row.names = FALSE)

mkPredC <- function(outCol,varCol,appCol) {     # Note: 1 
  pos <- 1
  pPos <- sum(outCol==pos)/length(outCol)      # Note: 2 
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]   # Note: 3 
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)    # Note: 4 
  pred <- pPosWv[appCol]       # Note: 5 
  pred[is.na(appCol)] <- pPosWna       # Note: 6 
  pred[is.na(pred)] <- pPos    # Note: 7 
  pred         # Note: 8 
}

mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol,
                                     probs=seq(0, 1, 0.1),na.rm=T)))
  varC <- cut(varCol,cuts)
  appC <- cut(appCol,cuts)
  mkPredC(outCol,varC,appC)
}

vars <- as.data.frame(fread('Feature_Engineering.csv'))
catvars <- vars$Vars[vars$Type == 'factor']
numvars <- vars$Vars[vars$Type == 'numeric']

for (v in catvars) {
  pi <- paste0('pred', v)
  train[,v] <- as.factor(train[,v])
  test[,v] <- as.factor(test[,v])
  train[,pi] <- mkPredC(train[,"TARGET"],train[,v],train[,v])
  test[,pi] <- mkPredC(train[,"TARGET"],train[,v],test[,v])
}

for (v in numvars) {
  pi <- paste0('pred', v)
  train[,pi] <- mkPredN(train[,"TARGET"],train[,v],train[,v])
  test[,pi] <- mkPredN(train[,"TARGET"],train[,v],test[,v])
}

train <- select(train, starts_with('pred'), TARGET)
test <- select(test, starts_with('pred'))

write.csv(train, 'train_mem.csv', row.names = FALSE)
write.csv(test, 'test_mem.csv', row.names = FALSE)

initTest <- function(TrainFile, action, params){
library(SDR)
  
train = read.keel(TrainFile)
mtrain <-t(simplify2array(train$data))
nTrain = ncol(mtrain)
train.classnames = train$class_names
train.features = train$atributeNames[-nTrain]
train.vars = mtrain[ , -nTrain]
#colnames(train.vars) <- train.features

#test = read.keel(TestFile)
#mtest <- t(simplify2array(test$data))
#nTst = ncol(mtest)
#test.classnames = test$class_names
#test.features = test$atributeNames[-nTst]
#test.vars = mtest[ , -nTst]
#colnames(test.vars) <- test.features

set.seed(42)

#source("checkDataSet.R")
#result <- checkDataSet(mtest)
#print(result)
if (action == "Over")
{
  source("iOver.R")
  i <- iOver(mtrain, params[1], params[2], params[3], params[4])
}
else
{
  source("iUnder.R")
  i <- iUnder(mtrain, params[1], params[2], params[3], params[4])
}

#print(i)
#i<-cbind(i$X,i$Y)
#resultNew <- checkDataSet(i)
#print(resultNew)
return(i)
}
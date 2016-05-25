readData <- function(trainSet, testSet) {
  library(SDR);
  training = read.keel(trainSet)
  test = read.keel(testSet)
  mtrain <- t(simplify2array(training$data))
  mtest <- t(simplify2array(test$data))
  return(list(trainSet = mtrain, testSet = mtest, n = ncol(mtrain)))
}
iOver <- function(dataset, safe, borderline, rare, outlier) {
  
  source("checkDataSet.R")
  testResult <- checkDataSet(dataset)
  
  print(testResult)
  
  X <- testResult$X
  Y <- testResult$Y
  
  if(testResult$score$minority.class == 0) {
    Y <- (Y -1)*(-1)
  }
  
  indexes <- 0
  if(safe) {
    indexes <- c(indexes, which(testResult$types == 1))
  }
  if(borderline) {
    indexes <- c(indexes, which(testResult$types == 2))
  }
  if(rare){
    indexes <- c(indexes, which(testResult$types == 3))
  }
  if(outlier) {
    indexes <- c(indexes, which(testResult$types == 4))
  }
  
  indexes <- indexes[-1]
  
  X <- X[indexes, ]
  Y <- Y[indexes]
  
  library(unbalanced)
  data <- ubOver(X = X, Y = Y)
  
  if(testResult$score$minority.class == 0) {
    data$Y <- (data$Y -1)*(-1)
  }
  return(data)
}
  


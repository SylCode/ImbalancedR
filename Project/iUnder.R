iUnder <- function(dataset, safe, borderline, rare, outlier) {
  
  source("checkDataSet.R")
  testResult <- checkDataSet(dataset)
  
  X <- testResult$X
  Y <- testResult$Y
  types <- testResult$types
  
  if(testResult$score$minority.class == 0) {
    Y <- (Y -1)*(-1)
  }
  
  indexes <- which(testResult$Y == 1)
  
  min.X <- X[indexes, ]
  min.Y <- Y[indexes]
  min.types <- types[indexes]
  
  maj.X <- X[-indexes, ]
  maj.Y <- Y[-indexes]
  
  indexes <- 0
  if(safe) {
    indexes <- c(indexes, which(min.types == 1))
  }
  if(borderline) {
    indexes <- c(indexes, which(min.types == 2))
  }
  if(rare){
    indexes <- c(indexes, which(min.types == 3))
  }
  if(outlier) {
    indexes <- c(indexes, which(min.types == 4))
  }
  
  indexes <- indexes[-1]
  
  min.X <- min.X[indexes, ]
  min.Y <- min.Y[indexes]
  
  X <- rbind(maj.X, min.X)
  Y <- c(maj.Y, min.Y)
  
  library(unbalanced)
  data <- ubUnder(X = X, Y = Y)
  
  if(testResult$score$minority.class == 0) {
    data$Y <- (data$Y -1)*(-1)
  }
  return(data)
}


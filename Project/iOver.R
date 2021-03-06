iOver <- function(X, Y, minority.class, types, safe, borderline, rare, outlier) {
  
  if(minority.class == 0) {
    Y <- (Y -1)*(-1)
  }
  
  indexes <- which(Y == 1)
  
  min.X <- X[indexes, ]
  min.Y <- Y[indexes]
  min.types <- types[indexes]
  
  maj.X <- X[-indexes, ]
  maj.Y <- Y[-indexes]
  
  N_Safe <- 0
  N_Borderline <- 0
  N_Rare <- 0
  N_Outlier <- 0
  indexes <- 0
  
  if(safe) {
    indexes <- c(indexes, which(min.types == 1))
    N_Safe = length(which(min.types == 1))
  }
  if(borderline) {
    indexes <- c(indexes, which(min.types == 2))
    N_Borderline = length(which(min.types == 2))
  }
  if(outlier){
    indexes <- c(indexes, which(min.types == 3))
    N_Outlier = length(which(min.types == 3))
  }
  if(rare) {
    indexes <- c(indexes, which(min.types == 4))
    N_Rare = length(which(min.types == 4))
  }
  
  indexes <- indexes[-1]
  
  if(length(indexes) > 1) {
    min.X <- min.X[indexes, ]
    min.Y <- min.Y[indexes]
    
    X <- rbind(maj.X, min.X)
    Y <- c(maj.Y, min.Y)
    
    library(unbalanced)
    data <- ubOver(X = X, Y = Y, k = 0)
    
  } else {
    
    min.Y <- rep(1, length(maj.Y))
    min.X <- min.X[min.Y, ]
    X <- rbind(maj.X, min.X)
    Y <- c(maj.Y, min.Y)
    data <- list(X = X, Y = Y)
  }
  
  if(minority.class == 0) {
    data$Y <- (data$Y -1)*(-1)
  }  
  
  data$N_Safe <- N_Safe
  data$N_Borderline <- N_Borderline
  data$N_Rare <- N_Rare
  data$N_Outlier <- N_Outlier
  data$N_Minority <- length(min.Y)
  data$N_Majority <- length(maj.Y)
  data$minority.class <- minority.class
  return(data)
}

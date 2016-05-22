iUnder <- function(ds, safe, borderLine, outlier, rare) {
  
  source("checkDataSet.R")
  info <- checkDataSet(ds)
  
  source("getNewSubset.R")
  min = getNewSubset(info, safe, borderLine, outlier, rare, 0)
  
  source("getNewSet.R")
  #maj = getNewSubset(info, safe, borderLine, outlier, rare, 1)
  maj = getNewSet(info, 1)
  
  Y <- c(maj$y, min$y)
  Y <- (Y-1)*(-1)
  X <- rbind(maj$x, min$x)
  #X <- c(maj$x, min$x)
  
  library(unbalanced)
  return(ubUnder(X, Y))
}
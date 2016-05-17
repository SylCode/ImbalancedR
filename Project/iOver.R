iOver <- function(ds, s) {
  
  source("checkDataSet.R")
  info <- checkDataSet(ds)
  
  source("getNewSubset.R")
  min = getNewSubset(info, s, 0)
  
  source("getNewSet.R")
  maj = getNewSet(info, 1)
  
  Y <- c(maj$y, min$y)
  X <- c(maj$x, min$x)

  library(unbalanced)
  return(ubOver(X, Y))
}
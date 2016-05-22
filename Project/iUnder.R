<<<<<<< HEAD
iUnder <- function(ds, safe, borderLine, outlier, rare) {
=======
iUnder <- function(ds, c0, c1) {
>>>>>>> 130232650f87d5d7f219215a55b10faae1483146
  
  source("checkDataSet.R")
  info <- checkDataSet(ds)
  
  source("getNewSubset.R")
<<<<<<< HEAD
  min = getNewSubset(info, safe, borderLine, outlier, rare, 0)
  
  source("getNewSet.R")
  #maj = getNewSubset(info, safe, borderLine, outlier, rare, 1)
  maj = getNewSet(info, 1)
  
  Y <- c(maj$y, min$y)
  Y <- (Y-1)*(-1)
  X <- rbind(maj$x, min$x)
  #X <- c(maj$x, min$x)
=======
  min = getNewSubset(info, c0, 0)
  
  source("getNewSet.R")
  maj = getNewSubset(info, c1, 1)
  
  Y <- c(maj$y, min$y)
  X <- c(maj$x, min$x)
>>>>>>> 130232650f87d5d7f219215a55b10faae1483146
  
  library(unbalanced)
  return(ubUnder(X, Y))
}
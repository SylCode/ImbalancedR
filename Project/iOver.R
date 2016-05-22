<<<<<<< HEAD
iOver <- function(dataSet, safe, borderLine, outlier, rare) {
  
  source("checkDataSet.R")
  info <- checkDataSet(dataSet)
  #print("Info_iOver")
  #print(info)
  
  source("getNewSubset.R")
  min = getNewSubset(info, safe, borderLine, outlier, rare, 0)
  #print("min_iOver")
  #print(min)
  
  source("getNewSet.R")
  maj = getNewSet(info, 1)
  #print("maj_iOver")
  #print(maj)
  
  Y <- c(maj$y, min$y)
  Y <- (Y-1)*(-1)
  X <- rbind(maj$x, min$x)
  #X<-maj$x
  #X<-min$x
  
  
  #print("X_iOver")
  #print(X)
  #print("Y_iOver")
  #print(Y)

  library(unbalanced)
  temp = ubOver(X,Y)
  temp$Y - (temp$Y-1)*(-1)
  
=======
iOver <- function(ds, c0, c1) {
  
  source("checkDataSet.R")
  info <- checkDataSet(ds)
  
  source("getNewSubset.R")
  min = getNewSubset(info, c0, 0)
  
  source("getNewSet.R")
  maj = getNewSubset(info, c1, 1)
  
  Y <- c(maj$y, min$y)
  X <- c(maj$x, min$x)

  library(unbalanced)
>>>>>>> 130232650f87d5d7f219215a55b10faae1483146
  return(ubOver(X, Y))
}
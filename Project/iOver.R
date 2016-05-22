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
  
  return(ubOver(X, Y))
}
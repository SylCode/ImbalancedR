C50 <- function(data, test.X) {
  library(C50)
  train.Y = as.factor(data$Y)
  train.X = as.data.frame(data$X)
  
  # model drzewa
  treemodel <- C5.0(x = train.X, y = train.Y)
  test.X <- as.data.frame(test.X)
  
  # predykcja
  result <- predict(treemodel, test.X)
  result <- as.numeric(levels(result))[result]
  return(result)
}
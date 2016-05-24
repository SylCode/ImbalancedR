testKnn <- function(data, test.X) {
  train.Y = as.factor(data$Y)
  train.X = as.data.frame(data$X)
  result <- knn(train.X, test.X, train.Y, k=5, prob = TRUE)
  result<- as.numeric(levels(result))[result]
  return(result)
}
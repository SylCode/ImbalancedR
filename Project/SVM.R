library("e1071")

mySVM <- function(data, test.X) {
  train.Y = data$Y
  train.X = data$X
  model <- svm(x = train.X, y = train.Y, type="C-classification")
  result <- predict(model, test.X)
  result <- as.numeric(levels(result))[result]
  return(result)
}
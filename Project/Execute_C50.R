Execute_C50 <- function( train, test)
{
  library(C50)
  source("checkResult.R")
  train.output = as.factor(train$Y)
  train.input = as.data.frame(train$X)
  # model drzewa
  treemodel <- C5.0(x = train.input, y = train.output)
  summary(treemodel)
  test.output <- as.factor(test$Y)
  test.input <- as.data.frame(test$X)
  # predykcja
  result <- predict(treemodel, test.input)
  result <- as.numeric(levels(result))[result]
  result <- (result -1)*(-1)
  
  #print ("C50")
  #print(result)
  #print ("True")
  #print (test.output)
  
  #print(result)
  return (checkResult(result, test.output))
}
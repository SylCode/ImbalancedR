computeConfusionMatrix <- function(result, Y, minority.class) {
  n <- length(result)
  tp <- 0
  tn <- 0
  fn <- 0
  fp <- 0
  
  for(i in 1:n) {
    if(result[i] == Y[i] && Y[i] == minority.class) {
      tp <- tp + 1 
    } else if(result[i] != Y[i] && result[i] == minority.class) {
      fp <- fp + 1
    } else if(result[i] != Y[i] && result[i] != minority.class) {
      fn <- fn + 1
    } else {
      tn <- tn + 1
    }
  }
  
  return(list(TP = tp, TN = tn, FP = fp, FN = fn))
}
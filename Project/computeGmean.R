computeGmean <- function(result, Y) {
  n <- length(result)
  tp <- 0
  fn <- 0
  fp <- 0
  
  for(i in 1:n) {
    if(result[i] == Y[i] && result[i] == 0) {
      tp <- tp + 1 
    } else if(result[i] != Y[i] && result[i] == 0) {
      fp <- fp + 1
    } else if(result[i] != Y[i] && result[i] == 1) {
      fn <- fn + 1
    }
  }
  
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  
  gmean <- sqrt(precision*recall)
  return(gmean)
}
  
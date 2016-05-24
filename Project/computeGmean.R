computeGmean <- function(tp, fp, fn) {
  
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  
  gmean <- sqrt(precision*recall)
  return(gmean)
}
  
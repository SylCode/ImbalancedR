getNewSet <- function(info, c) {
 
  idx <- which(info$classes == c)
  x <- info$features[idx,]
  y <- info$classes[idx]
  
  return(list(y = y, x = x))
}
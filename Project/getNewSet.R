getNewSet <- function(info, c) {
 
  idx <- which(info$classes == c)
  x <- info$features[maj.idx, ]
  y <- info$classes[maj.idx]
  
  return(list(y = y, x = x))
}
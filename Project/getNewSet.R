getNewSet <- function(info, c) {
 
  idx <- which(info$classes == c)
<<<<<<< HEAD
  x <- info$features[idx,]
  y <- info$classes[idx]
=======
  x <- info$features[maj.idx, ]
  y <- info$classes[maj.idx]
>>>>>>> 130232650f87d5d7f219215a55b10faae1483146
  
  return(list(y = y, x = x))
}
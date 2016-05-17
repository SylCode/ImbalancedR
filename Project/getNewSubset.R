getNewSubset <- function(info, s, c) {
  idx <- which(info$classes == c)
  
  features <- info$features[idx, ]
  classes <- info$classes[idx]
  types <- info$types[idx]
  
  i <- c(0)
  
  print(types)
  
  if(s & 1 != 0) {
    i <- c(i, which(types == 1))
  }
  
  if(s & 2 != 0) {
    i <- c(i, which(types == 2))
  }
  
  if(s & 4 != 0) {
    i <- c(i, which(types == 3))
  }
  
  if(s & 8 != 0) {
    i <- c(i, which(types == 4))
  }
  
  i <- i[-1]
  
  y = classes[i]
  x = features[i]
  
  return(list(y = y, x = x))
}
<<<<<<< HEAD
getNewSubset <- function(info, safe, borderLine, outlier, rare , c) {
  indexes <- which(info$classes == c)
  
  features <- info$features[indexes,]
  #print("Features_getNewSubset")
  #print(features)
  
  classes <- info$classes[indexes]
  #print("classes_getNewSubset")
  #print(classes)
  
  types <- info$types[indexes]
  #print("types_getNewSubset")
  #print(types)
  
  i <- c(0)
  
  
  if(safe ) {
    i <- c(i, which(types == 1))
  }
  
  if(borderLine) {
    i <- c(i, which(types == 2))
  }
  
  if(outlier) {
    i <- c(i, which(types == 3))
  }
  
  if(rare) {
=======
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
>>>>>>> 130232650f87d5d7f219215a55b10faae1483146
    i <- c(i, which(types == 4))
  }
  
  i <- i[-1]
  
  y = classes[i]
<<<<<<< HEAD
  x = features[i,]
=======
  x = features[i]
>>>>>>> 130232650f87d5d7f219215a55b10faae1483146
  
  return(list(y = y, x = x))
}
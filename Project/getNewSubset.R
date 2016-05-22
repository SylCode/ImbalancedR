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
    i <- c(i, which(types == 4))
  }
  
  i <- i[-1]
  
  y = classes[i]
  x = features[i,]
  
  return(list(y = y, x = x))
}
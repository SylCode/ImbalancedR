checkDataSet <- function(ds){
  library(class)
  m <- nrow(ds)
  n <- ncol(ds)
  classes <- ds[1:m, n]
  m1 <- length(which(classes == 1))
  m2 <- length(which(classes != 1)) 
  features <- ds[1:m, -n]
  score <- data.frame(safe = 0, unsafe = 0, borderline = 0, rare = 0, outlier = 0, safe1 = 0, unsafe1 = 0, borderline1 = 0, rare1 = 0, outlier1 = 0, safe2 = 0, unsafe2 = 0, borderline2 = 0, rare2 = 0, outlier2 = 0)
  for (i in 1:m) {
    test.class <- classes[i]
    test.features <- features[i, 1:n-1]
    train.classes <- factor(classes[-i])
    train.features <- features[-i, 1:n-1]
    result.factor <- knn(train.features, test.features, train.classes, k=5, prob = TRUE)
    result.class <- as.numeric(levels(result.factor))[result.factor]
    result.prob <- attr(result.factor,"prob")

    if(result.class == test.class) {
      # jesli jednoznaczna klasyfikacja to safe
      if(result.prob == 1) {
        score$safe <- score$safe + 1
        if (test.class ==1){
          score$safe1 <- score$safe1 + 1
        }
        else{
          score$safe2 <- score$safe2 + 1
        }
      # jesli zaklasyfikowane dobrze, ale nie jednoglosna decyzja to borderline
      } else {
        score$borderline <- score$borderline + 1
        score$unsafe <- score$unsafe + 1
        if (test.class ==1){
          score$borderline1 <- score$borderline1 + 1
          score$unsafe1 <- score$unsafe1 + 1
        }
        else{
          score$borderline2 <- score$borderline2 + 1
          score$unsafe2 <- score$unsafe2 + 1
        }
      }
    } else {
      # jesli zaklasyfikowane niepoprawnie i jednoglosna decyzja to outlier
      if(result.prob == 1) {
        score$outlier <- score$outlier + 1
        score$unsafe <- score$unsafe + 1
        if (test.class ==1){
          score$outlier1 <- score$outlier1 + 1
          score$unsafe1 <- score$unsafe1 + 1
        }
        else{
          score$outlier2 <- score$outlier2 + 1
          score$unsafe2 <- score$unsafe2 + 1
        }
      # jesli zaklasyfikowane niepoprawnie i niejednoglosna decyzja to rare
      } else {
        score$rare <- score$rare + 1
        score$unsafe <- score$unsafe + 1
        if (test.class ==1){
          score$rare1 <- score$rare1 + 1
          score$unsafe1 <- score$unsafe1 + 1
        }
        else{
          score$rare2 <- score$rare2 + 1
          score$unsafe2 <- score$unsafe2 + 1
        }
      }
    }
  }
  return(score)
}
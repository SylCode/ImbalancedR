checkDataSet <- function(ds){
  library(class)
  m <- nrow(ds)
  n <- ncol(ds)
  classes <- ds[1:m, n]
  features <- ds[1:m, -n]
  score <- data.frame(safe = 0, unsafe = 0, borderline = 0, rare = 0, outlier = 0)
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
      # jesli zaklasyfikowane dobrze, ale nie jednoglosna decyzja to borderline
      } else {
        score$borderline <- score$borderline + 1
        score$unsafe <- score$unsafe + 1
      }
    } else {
      # jesli zaklasyfikowane niepoprawnie i jednoglosna decyzja to outlier
      if(result.prob == 1) {
        score$outlier <- score$outlier + 1
        score$unsafe <- score$unsafe + 1
      # jesli zaklasyfikowane niepoprawnie i niejednoglosna decyzja to rare
      } else {
        score$rare <- score$rare + 1
        score$unsafe <- score$unsafe + 1
      }
    }
  }
  return(score)
}
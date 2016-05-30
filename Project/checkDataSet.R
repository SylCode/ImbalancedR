checkDataSet <- function(ds){
  library(class)
  
  # wymiary danych
  m <- nrow(ds)
  n <- ncol(ds)
  
  # ostatni rzad klasy
  classes <- ds[1:m, n]
  
  types <- rep(0, m)
  
  # ile obiektow z ktorej klasy
  class1 <- length(which(classes == 1))
  class2 <- length(which(classes == 0)) 
  
  # przypisanie numerow klas
  if(class1 > class2) {
    majority.class <- 1;
    minority.class <- 0;
    minority.count <- class2;
    majority.count <- class1;
  } else {
    majority.class <- 0;
    minority.class <- 1;
    minority.count <- class1;
    majority.count <- class2;
  }
  
  # cechy z klasy
  features <- ds[1:m, -n]
  score <- data.frame(minority.class = minority.class, majority.class = majority.class, minority.count = minority.count, majority.count = majority.count,
                      safe = 0, unsafe = 0, borderline = 0, rare = 0, outlier = 0, 
                      minority.safe = 0, minority.unsafe = 0, minority.borderline = 0, minority.rare = 0, minority.outlier = 0, 
                      majority.safe = 0, majority.unsafe = 0, majority.borderline = 0, majority.rare = 0, majority.outlier = 0)
  
  
  for (i in 1:m) {
    
    # wczytanie klasy dla badanego rekordu
    test.class <- classes[i]
    
    # wczytanie cech dla bradanego rekordu
    test.features <- features[i, 1:n-1]
    
    # wektor z klasami bez badanego rekordu
    train.classes <- factor(classes[-i])
    
    # cechy bez badanego rekordu
    train.features <- features[-i, 1:n-1]
    
    # knn
    result.factor <- knn(train.features, test.features, train.classes, k=5, prob = TRUE)
    result.class <- as.numeric(levels(result.factor))[result.factor]
    result.prob <- attr(result.factor,"prob")
    
    if(result.class == test.class) {
      # jesli jednoznaczna klasyfikacja to safe
      if(result.prob == 1) {
        score$safe <- score$safe + 1
        types[i] <- 1
        if (test.class == minority.class){
          score$minority.safe <- score$minority.safe + 1
        }
        else{
          score$majority.safe <- score$majority.safe + 1
        }
        
        # jesli zaklasyfikowane dobrze, ale nie jednoglosna decyzja to borderline
      } else {
        types[i] <- 2
        score$borderline <- score$borderline + 1
        score$unsafe <- score$unsafe + 1
        if (test.class == minority.class){
          score$minority.borderline <- score$minority.borderline + 1
          score$minority.unsafe <- score$minority.unsafe + 1
        }
        else{
          score$majority.borderline <- score$majority.borderline + 1
          score$majority.unsafe <- score$majority.unsafe + 1
        }
      }
      
    } else {
      # jesli zaklasyfikowane niepoprawnie i jednoglosna decyzja to outlier
      if(result.prob == 1) {
        types[i] <- 3
        score$outlier <- score$outlier + 1
        score$unsafe <- score$unsafe + 1
        if (test.class == minority.class){
          score$minority.outlier <- score$minority.outlier + 1
          score$minority.unsafe <- score$minority.unsafe + 1
        }
        else{
          score$majority.outlier <- score$majority.outlier + 1
          score$majority.unsafe <- score$majority.unsafe + 1
        }
        # jesli zaklasyfikowane niepoprawnie i niejednoglosna decyzja to rare
      } else {
        types[i] <- 4
        score$rare <- score$rare + 1
        score$unsafe <- score$unsafe + 1
        if (test.class == minority.class){
          score$minority.rare <- score$minority.rare + 1
          score$minority.unsafe <- score$minority.unsafe + 1
        }
        else{
          score$majority.rare <- score$majority.rare + 1
          score$majority.unsafe <- score$majority.unsafe + 1
        }
      }
    }
  }
  
  result <- list(score = score, X = features, Y = classes, types = types)
  return(result)
}
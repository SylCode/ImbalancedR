source("readData.R")
input <- c("DataSets/dermatology-6-5-fold (IR 16.9)/dermatology-6-5-",
           "DataSets/ecoli4-5-fold (IR 15.8)/ecoli4-5-",
           "DataSets/segment0-5-fold (IR 6.02)/segment0-5-",
           "DataSets/yeast5-5-fold (IR 32.73)/yeast5-5-")

nameSet <- c("dermatology",
            "ecoli4",
            "segment0",
            "yeast5")

values <- c(TRUE, FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE, 
            TRUE, TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, 
            TRUE, TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, 
            TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

choise <- matrix(values, 15)

write("Set;Safe;Outlier;Borderline;Rare;TP;TN;FP;FN;GMean", file = "results.csv", sep=";", append = TRUE)
for(j in 1:15) {
  TP <- 0
  TN <- 0
  FP <- 0
  FN <- 0
  
  for(i in 1:5) {
    source("readData.R")
    dataset <- readData(paste("DataSets/dermatology-6-5-fold (IR 16.9)/dermatology-6-5-",i,"tra.dat", sep=""), paste("DataSets/dermatology-6-5-fold (IR 16.9)/dermatology-6-5-",i,"tst.dat", sep=""))
    
    test.X <- dataset$testSet[ , -dataset$n]
    test.Y <- dataset$testSet[ , dataset$n]
    
    source("iOver.R")
    data <- iOver(dataset = dataset$trainSet, safe = choise[j, 1], borderline = choise[j, 2], rare = choise[j, 3], outlier = choise[j, 4])
    
    source("testKnn.R")
    result <- testKnn(data = data, test.X = test.X)
    
    source("computeConfusionMatrix.R")
    confusionMatrix <- computeConfusionMatrix(resul = result, Y = test.Y, minority.class = data$minority.class)
    
    TP <- TP + confusionMatrix$TP
    TN <- TN + confusionMatrix$TN
    FP <- FP + confusionMatrix$FP
    FN <- FN + confusionMatrix$FN
  }
  
  source("computeGmean.R")
  gmean <- computeGmean(tp = TP, fp = FP, fn = FN)
  
  write(paste("dermatology-6", choise[j, 1], choise[j, 2], choise[j, 3], choise[j, 4], TP, TN, FP, FN, gmean, sep=";"), file = "results.csv" , append = TRUE)
}
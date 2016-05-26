source("readData.R")
source("iOver.R")
source("Knn.R")
source("computeConfusionMatrix.R")
source("computeGmean.R")


output <- "Results/results_over_knn.csv"

input <- c("DataSets/wisconsin-5-fold (IR 1.86)/wisconsin-5-",
           "DataSets/dermatology-6-5-fold (IR 16.9)/dermatology-6-5-",
           "DataSets/ecoli4-5-fold (IR 15.8)/ecoli4-5-",
           "DataSets/kddcup-buffer_overflow_vs_back-5-fold (IR 73.43)/kddcup-buffer_overflow_vs_back-5-",
           "DataSets/kddcup-rootkit-imap_vs_back-5-fold (IR 100.14)/kddcup-rootkit-imap_vs_back-5-",
           "DataSets/segment0-5-fold (IR 6.02)/segment0-5-",
           "DataSets/winequality-red-8_vs_6-7-5-fold (IR 46.5)/winequality-red-8_vs_6-7-5-",
           "DataSets/yeast5-5-fold (IR 32.73)/yeast5-5-")

nameSet <- c("wisconsin",
            "dermatology-6",
            "ecoli4",
            "kddcup-buffer_overflow_vs_back",
            "kddcup-rootkit-imap_vs_back",
            "segment0",
            "winequality-red-8_vs_6-7",
            "yeast5")

values <- c(TRUE, FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE, 
            TRUE, TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, 
            TRUE, TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, 
            TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

choise <- matrix(values, 15)

write("Set;Safe;Borderline;Rare;Outlier;TP;TN;FP;FN;GMean", file = output, sep=";", append = TRUE)

for(k in 1:length(input)) {

  for(j in 1:nrow(choise)) {
    TP <- 0
    TN <- 0
    FP <- 0
    FN <- 0
    
    checkResults <- TRUE
    
    for(i in 1:5) {
      dataset <- readData(paste(input[k],i,"tra.dat", sep=""), paste(input[k],i,"tst.dat", sep=""))
      
      test.X <- dataset$testSet[ , -dataset$n]
      test.Y <- dataset$testSet[ , dataset$n]
      
      source("checkDataSet.R")
      testResult <- checkDataSet(dataset$trainSet)
      
      source("checkNumOfObjs.R")
      numOfObjs <- checkNumOfObjs(testResult = testResult, safe = choise[j, 1], borderline = choise[j, 2],
                                  rare = choise[j, 3], outlier = choise[j, 4]) 
      
      if(numOfObjs == 0) {
        checkResults <- FALSE
        break
      }
      
      data <- iOver(X = testResult$X, Y = testResult$Y, minority.class = testResult$score$minority.class, types = testResult$types, 
                    safe = choise[j, 1], borderline = choise[j, 2], rare = choise[j, 3], outlier = choise[j, 4])
      
      result <- Knn(data = data, test.X = test.X)
      
      confusionMatrix <- computeConfusionMatrix(result = result, Y = test.Y, minority.class = data$minority.class)
      
      TP <- TP + confusionMatrix$TP
      TN <- TN + confusionMatrix$TN
      FP <- FP + confusionMatrix$FP
      FN <- FN + confusionMatrix$FN
    }
    
    if(checkResults) {
      gmean <- computeGmean(tp = TP, fp = FP, fn = FN)
      
      write(paste(nameSet[k], choise[j, 1], choise[j, 2], choise[j, 3], choise[j, 4], 
                  TP, TN, FP, FN, gmean, sep=";"), file = output , append = TRUE)
    } else {
      write(paste(nameSet[k], choise[j, 1], choise[j, 2], choise[j, 3], choise[j, 4], 
                  "-", "-", "-", "-", "-", sep=";"), file = output , append = TRUE)
    }
  }
  
}
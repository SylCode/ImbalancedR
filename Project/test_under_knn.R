source("readData.R")
source("iUnder.R")
source("Knn.R")
source("computeConfusionMatrix.R")
source("computeGmean.R")
source("getROC_AUC.R")

output <- "Results/results_under_knn.csv"

input <- c("DataSets/wisconsin-5-fold (IR 1.86)/wisconsin-5-",
           #"DataSets/dermatology-6-5-fold (IR 16.9)/dermatology-6-5-",
           #"DataSets/ecoli4-5-fold (IR 15.8)/ecoli4-5-",
           #"DataSets/kddcup-buffer_overflow_vs_back-5-fold (IR 73.43)/kddcup-buffer_overflow_vs_back-5-",
           #"DataSets/kddcup-rootkit-imap_vs_back-5-fold (IR 100.14)/kddcup-rootkit-imap_vs_back-5-",
           #"DataSets/segment0-5-fold (IR 6.02)/segment0-5-",
           #"DataSets/winequality-red-8_vs_6-7-5-fold (IR 46.5)/winequality-red-8_vs_6-7-5-",
           "DataSets/yeast5-5-fold (IR 32.73)/yeast5-5-")

nameSet <- c("wisconsin",
             #"dermatology-6",
             #"ecoli4",
             #"kddcup-buffer_overflow_vs_back",
             #"kddcup-rootkit-imap_vs_back",
             #"segment0",
             #"winequality-red-8_vs_6-7",
             "yeast5")

values <- c(TRUE, FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE,  FALSE, TRUE, 
            TRUE, TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE,  FALSE, 
            TRUE, TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, 
            TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

choise <- matrix(values, 15)

write("Set;Safe;Borderline;Rare;Outlier;N_Safe;Nnew_Safe;N_Borderline;Nnew_Borderline;N_Rare;Nnew_Rare;N_Outlier;Nnew_Outlier;N_Minority;Nnew_Minority;N_Majority;Nnew_Majority;IR;newIR;TP;TN;FP;FN;GMean;AUC", file = output, sep=";", append = TRUE)

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
      
      data <- iUnder(X = testResult$X, Y = testResult$Y, minority.class = testResult$score$minority.class, types = testResult$types, 
                    safe = choise[j, 1], borderline = choise[j, 2], rare = choise[j, 3], outlier = choise[j, 4])
      
      testData = cbind(data$X,data$Y)
      testResult <- checkDataSet(testData)
      
      N_Safe <- data$N_Safe
      N_Borderline <- data$N_Borderline
      N_Rare <- data$N_Rare
      N_Outlier <- data$N_Outlier
      N_Minority <- data$N_Minority
      N_Majority <- data$N_Majority
      IR <- N_Majority/N_Minority
      
      Nnew_Safe <- testResult$score$minority.safe
      Nnew_Borderline <- testResult$score$minority.borderline
      Nnew_Rare <- testResult$score$minority.rare
      Nnew_Outlier <- testResult$score$minority.outlier
      Nnew_Minority <- testResult$score$minority.count
      Nnew_Majority <- testResult$score$majority.count
      newIR <- Nnew_Majority/Nnew_Minority
      
      result <- Knn(data = data, test.X = test.X)
      
      confusionMatrix <- computeConfusionMatrix(result = result, Y = test.Y, minority.class = data$minority.class)
      aList = getROC_AUC(result, test.Y) 
      auc = unlist(aList$auc)
      
      #Wykrec ROC i AUC
      stack_x = unlist(aList$stack_x)
      stack_y = unlist(aList$stack_y)
      
      plot(stack_x, stack_y, type = "l", col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC")
      axis(1, seq(0.0,1.0,0.1))
      axis(2, seq(0.0,1.0,0.1))
      abline(h=seq(0.0,1.0,0.1), v=seq(0.0,1.0,0.1), col="gray", lty=3)
      legend(0.7, 0.3, sprintf("%3.3f",auc), lty=c(1,1), lwd=c(2.5,2.5), col="blue", title = "AUC")
      
      TP <- TP + confusionMatrix$TP
      TP <- TP + confusionMatrix$TP
      TN <- TN + confusionMatrix$TN
      FP <- FP + confusionMatrix$FP
      FN <- FN + confusionMatrix$FN
      
      
    }
    
    if(checkResults) {
      gmean <- computeGmean(tp = TP, fp = FP, fn = FN)
      
      write(paste(nameSet[k], choise[j, 1], choise[j, 2], choise[j, 3], choise[j, 4], N_Safe,Nnew_Safe,N_Borderline,Nnew_Borderline,N_Rare,Nnew_Rare,N_Outlier,Nnew_Outlier,N_Minority,Nnew_Minority,N_Majority,Nnew_Majority,IR,newIR,
                  TP, TN, FP, FN, gmean, auc, sep=";"), file = output, append = TRUE)
    } else {
      write(paste(nameSet[k], choise[j, 1], choise[j, 2], choise[j, 3], choise[j, 4], 
                  "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", sep=";"), file = output, append = TRUE)
    }
  }
  
}
library(SDR);
source("readData.R")
input <- c("DataSets/wisconsin-5-fold (IR 1.86)/wisconsin-5-",
           "DataSets/dermatology-6-5-fold (IR 16.9)/dermatology-6-5-",
           "DataSets/ecoli4-5-fold (IR 15.8)/ecoli4-5-",
           "DataSets/kddcup-buffer_overflow_vs_back-5-fold (IR 73.43)/kddcup-buffer_overflow_vs_back-5-",
           "DataSets/kddcup-rootkit-imap_vs_back-5-fold (IR 100.14)/kddcup-rootkit-imap_vs_back-5-",
           "DataSets/segment0-5-fold (IR 6.02)/segment0-5-",
           "DataSets/winequality-red-8_vs_6-7-5-fold (IR 46.5)/winequality-red-8_vs_6-7-5-",
           "DataSets/yeast5-5-fold (IR 32.73)/yeast5-5-")

setName <- c("winsconsin",
            "dermatology",
            "ecoli4",
            "kddcup-buffer_overflow_vs_back",
            "kddcup-rootkit-imap_vs_back",
            "segment0",
            "winequality-red",
            "yeast5")

output <- c("Results/knn-over-tttt.csv",
            "Results/knn-over-fttt.csv",
            "Results/knn-over-tftt.csv",
            "Results/knn-over-fftt.csv",
            "Results/knn-over-ttft.csv",
            "Results/knn-over-ftft.csv",
            "Results/knn-over-tfft.csv",
            "Results/knn-over-ffft.csv",
            "Results/knn-over-tttf.csv",
            "Results/knn-over-fttf.csv",
            "Results/knn-over-tftf.csv",
            "Results/knn-over-fftf.csv",
            "Results/knn-over-ttff.csv",
            "Results/knn-over-ftff.csv",
            "Results/knn-over-tfff.csv")

values <- c(TRUE, TRUE, TRUE, TRUE,
            FALSE, TRUE, TRUE, TRUE,
            TRUE, FALSE, TRUE, TRUE,
            FALSE, FALSE, TRUE, TRUE,
            TRUE, TRUE, FALSE, TRUE,
            FALSE, TRUE, FALSE, TRUE,
            TRUE, FALSE, FALSE, TRUE,
            FALSE, FALSE, FALSE, TRUE,
            TRUE, TRUE, TRUE, FALSE,
            FALSE, TRUE, TRUE, FALSE,
            TRUE, FALSE, TRUE, FALSE,
            FALSE, FALSE, TRUE, FALSE,
            TRUE, TRUE, FALSE, FALSE,
            FALSE, TRUE, FALSE, FALSE,
            TRUE, FALSE, FALSE, FALSE)

choise <- matrix(values, 15)

for(l in 1:15) {
  write("SET; TP; TN; FP; FN; GMEAN; ", file = output[l], append = TRUE);
  for(k in 1:8) {
    print(output[k])
    TPSUM <- 0
    TNSUM <- 0
    FPSUM <- 0
    FNSUM <- 0
    for(i in 1:5) {
      trainsetFile <- paste(input[k], i, "tra.dat", sep="")
      testsetFile <- paste(input[k], i, "tst.dat", sep="")
      dataset <- readData(trainsetFile, testsetFile)
      
      TP <- 0
      TN <- 0
      FP <- 0
      FN <- 0
      
      for(j in 1:10) {
        test.X <- dataset$testSet[ , -dataset$n]
        test.Y <- dataset$testSet[ , dataset$n]
        
        source("iOver.R")
        data <- iOver(dataset = dataset$trainSet, safe = choise[l, 1], borderline = choise[l, 2], rare = choise[l, 3], outlier = choise[l, 4])
        
        source("testKnn.R")
        result <- testKnn(data = data, test.X = test.X)
        source("computeConfusionMatrix.R")
        confusionMatrix <- computeConfusionMatrix(resul = result, Y = test.Y)
        TP <- TP + confusionMatrix$TP
        TN <- TN + confusionMatrix$TN
        FP <- FP + confusionMatrix$FP
        FN <- FN + confusionMatrix$FN
      }
      
      source("computeGmean.R")
      gmean <- computeGmean(tp = TP, fp = FP, fn = FN)
      
      TPSUM <- TPSUM + TP
      TNSUM <- TNSUM + TN
      FPSUM <- FPSUM + FP
      FNSUM <- FNSUM + FN
    }
    
    source("computeGmean.R")
    gmean <- computeGmean(tp = TPSUM, fp = FPSUM, fn = FNSUM)
    write(paste(setName[k], TPSUM, TNSUM, FPSUM, FNSUM, gmean, sep = ";"), file = output[l], append = TRUE);
  }
}

output <- c("Results/C50-over-tttt.csv",
            "Results/C50-over-fttt.csv",
            "Results/C50-over-tftt.csv",
            "Results/C50-over-fftt.csv",
            "Results/C50-over-ttft.csv",
            "Results/C50-over-ftft.csv",
            "Results/C50-over-tfft.csv",
            "Results/C50-over-ffft.csv",
            "Results/C50-over-tttf.csv",
            "Results/C50-over-fttf.csv",
            "Results/C50-over-tftf.csv",
            "Results/C50-over-fftf.csv",
            "Results/C50-over-ttff.csv",
            "Results/C50-over-ftff.csv",
            "Results/C50-over-tfff.csv")

for(l in 1:15) {
  write("SET; TP; TN; FP; FN; GMEAN; ", file = output[l], append = TRUE);
  for(k in 1:8) {
    print(output[k])
    TPSUM <- 0
    TNSUM <- 0
    FPSUM <- 0
    FNSUM <- 0
    for(i in 1:5) {
      trainsetFile <- paste(input[k], i, "tra.dat", sep="")
      testsetFile <- paste(input[k], i, "tst.dat", sep="")
      dataset <- readData(trainsetFile, testsetFile)
      
      TP <- 0
      TN <- 0
      FP <- 0
      FN <- 0
      
      for(j in 1:10) {
        test.X <- dataset$testSet[ , -dataset$n]
        test.Y <- dataset$testSet[ , dataset$n]
        
        source("iOver.R")
        data <- iOver(dataset = dataset$trainSet, safe = choise[l, 1], borderline = choise[l, 2], rare = choise[l, 3], outlier = choise[l, 4])
        
        source("testC50.R")
        result <- testC50(data = data, test.X = test.X)
        source("computeConfusionMatrix.R")
        confusionMatrix <- computeConfusionMatrix(resul = result, Y = test.Y)
        TP <- TP + confusionMatrix$TP
        TN <- TN + confusionMatrix$TN
        FP <- FP + confusionMatrix$FP
        FN <- FN + confusionMatrix$FN
      }
      
      source("computeGmean.R")
      gmean <- computeGmean(tp = TP, fp = FP, fn = FN)
      
      TPSUM <- TPSUM + TP
      TNSUM <- TNSUM + TN
      FPSUM <- FPSUM + FP
      FNSUM <- FNSUM + FN
    }
    
    source("computeGmean.R")
    gmean <- computeGmean(tp = TPSUM, fp = FPSUM, fn = FNSUM)
    write(paste(setName[k], TPSUM, TNSUM, FPSUM, FNSUM, gmean, sep = ";"), file = output[l], append = TRUE);
  }
}

output <- c("Results/knn-under-tttt.csv",
            "Results/knn-under-fttt.csv",
            "Results/knn-under-tftt.csv",
            "Results/knn-under-fftt.csv",
            "Results/knn-under-ttft.csv",
            "Results/knn-under-ftft.csv",
            "Results/knn-under-tfft.csv",
            "Results/knn-under-ffft.csv",
            "Results/knn-under-tttf.csv",
            "Results/knn-under-fttf.csv",
            "Results/knn-under-tftf.csv",
            "Results/knn-under-fftf.csv",
            "Results/knn-under-ttff.csv",
            "Results/knn-under-ftff.csv",
            "Results/knn-under-tfff.csv")

for(l in 1:15) {
  write("SET; TP; TN; FP; FN; GMEAN; ", file = output[l], append = TRUE);
  for(k in 1:8) {
    print(output[k])
    TPSUM <- 0
    TNSUM <- 0
    FPSUM <- 0
    FNSUM <- 0
    for(i in 1:5) {
      trainsetFile <- paste(input[k], i, "tra.dat", sep="")
      testsetFile <- paste(input[k], i, "tst.dat", sep="")
      dataset <- readData(trainsetFile, testsetFile)
      
      TP <- 0
      TN <- 0
      FP <- 0
      FN <- 0
      
      for(j in 1:10) {
        test.X <- dataset$testSet[ , -dataset$n]
        test.Y <- dataset$testSet[ , dataset$n]
        
        source("iUnder.R")
        data <- iUnder(dataset = dataset$trainSet, safe = choise[l, 1], borderline = choise[l, 2], rare = choise[l, 3], outlier = choise[l, 4])
        
        source("testKnn.R")
        result <- testKnn(data = data, test.X = test.X)
        source("computeConfusionMatrix.R")
        confusionMatrix <- computeConfusionMatrix(resul = result, Y = test.Y)
        TP <- TP + confusionMatrix$TP
        TN <- TN + confusionMatrix$TN
        FP <- FP + confusionMatrix$FP
        FN <- FN + confusionMatrix$FN
      }
      
      source("computeGmean.R")
      gmean <- computeGmean(tp = TP, fp = FP, fn = FN)
      
      TPSUM <- TPSUM + TP
      TNSUM <- TNSUM + TN
      FPSUM <- FPSUM + FP
      FNSUM <- FNSUM + FN
    }
    
    source("computeGmean.R")
    gmean <- computeGmean(tp = TPSUM, fp = FPSUM, fn = FNSUM)
    write(paste(setName[k], TPSUM, TNSUM, FPSUM, FNSUM, gmean, sep = ";"), file = output[l], append = TRUE);
  }
}

output <- c("Results/C50-under-tttt.csv",
            "Results/C50-under-fttt.csv",
            "Results/C50-under-tftt.csv",
            "Results/C50-under-fftt.csv",
            "Results/C50-under-ttft.csv",
            "Results/C50-under-ftft.csv",
            "Results/C50-under-tfft.csv",
            "Results/C50-under-ffft.csv",
            "Results/C50-under-tttf.csv",
            "Results/C50-under-fttf.csv",
            "Results/C50-under-tftf.csv",
            "Results/C50-under-fftf.csv",
            "Results/C50-under-ttff.csv",
            "Results/C50-under-ftff.csv",
            "Results/C50-under-tfff.csv")

for(l in 1:15) {
  write("SET; TP; TN; FP; FN; GMEAN; ", file = output[l], append = TRUE);
  for(k in 1:8) {
    print(output[k])
    TPSUM <- 0
    TNSUM <- 0
    FPSUM <- 0
    FNSUM <- 0
    for(i in 1:5) {
      trainsetFile <- paste(input[k], i, "tra.dat", sep="")
      testsetFile <- paste(input[k], i, "tst.dat", sep="")
      dataset <- readData(trainsetFile, testsetFile)
      
      TP <- 0
      TN <- 0
      FP <- 0
      FN <- 0
      
      for(j in 1:10) {
        test.X <- dataset$testSet[ , -dataset$n]
        test.Y <- dataset$testSet[ , dataset$n]
        
        source("iUnder.R")
        data <- iUnder(dataset = dataset$trainSet, safe = choise[l, 1], borderline = choise[l, 2], rare = choise[l, 3], outlier = choise[l, 4])
        
        source("testC50.R")
        result <- testC50(data = data, test.X = test.X)
        source("computeConfusionMatrix.R")
        confusionMatrix <- computeConfusionMatrix(resul = result, Y = test.Y)
        TP <- TP + confusionMatrix$TP
        TN <- TN + confusionMatrix$TN
        FP <- FP + confusionMatrix$FP
        FN <- FN + confusionMatrix$FN
      }
      
      source("computeGmean.R")
      gmean <- computeGmean(tp = TP, fp = FP, fn = FN)
      
      TPSUM <- TPSUM + TP
      TNSUM <- TNSUM + TN
      FPSUM <- FPSUM + FP
      FNSUM <- FNSUM + FN
    }
    
    source("computeGmean.R")
    gmean <- computeGmean(tp = TPSUM, fp = FPSUM, fn = FNSUM)
    write(paste(setName[k], TPSUM, TNSUM, FPSUM, FNSUM, gmean, sep = ";"), file = output[l], append = TRUE);
  }
}

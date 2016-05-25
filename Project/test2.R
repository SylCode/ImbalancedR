source("readData.R")
dataset <- readData("DataSets/dermatology-6-5-fold (IR 16.9)/dermatology-6-5-5tra.dat", "DataSets/dermatology-6-5-fold (IR 16.9)/dermatology-6-5-5tst.dat")

test.X <- dataset$testSet[ , -dataset$n]
test.Y <- dataset$testSet[ , dataset$n]

source("iOver.R")
data <- iOver(dataset = dataset$trainSet, safe = TRUE, borderline = FALSE, rare = FALSE, outlier = TRUE)

source("testKnn.R")
result <- testKnn(data = data, test.X = test.X)

source("computeConfusionMatrix.R")
confusionMatrix <- computeConfusionMatrix(resul = result, Y = test.Y, minority.class = data$minority.class)

source("computeGmean.R")
gmean <- computeGmean(tp = confusionMatrix$TP, fp = confusionMatrix$FP, fn = confusionMatrix$FN)
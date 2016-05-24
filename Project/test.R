library(SDR);
source("readData.R")
dataset <- readData("DataSets/wisconsin-5-fold (IR 1.86)/wisconsin-5-1tra.dat", "DataSets/wisconsin-5-fold (IR 1.86)/wisconsin-5-1tst.dat")

# wyciagniecie z danych klasif(testResult$score$minority.class == 1) {
test.X <- dataset$testSet[ , -dataset$n]
test.Y <- dataset$testSet[ , dataset$n]

source("iOver.R")
data <- iOver(dataset = dataset$trainSet, safe = FALSE, borderline = TRUE, rare = TRUE, outlier = TRUE)

source("testC50.R")
result <- testC50(data = data, test.X = test.X)

source("computeGmean.R")
gmean <- computeGmean(result = result, Y = test.Y)


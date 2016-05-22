go <- function(genFile, params)
{
  library(SDR)
  for (i in 1:5) {
    
    trainSet = initTest(paste(genFile,i,"tra.dat",sep=""),"Over", params)
    testSet = read.keel(paste(genFile,i,"tst.dat",sep=""))
    
    testSet <- t(simplify2array(testSet$data))
    n = ncol(testSet)
    
    testSet.X <- testSet[ , -n]
    testSet.Y <- testSet[ , n]
    
    testSet <- list(X=testSet.X, Y=testSet.Y)
    gMean <- Execute_C50(trainSet, testSet)
    print (paste("Gmean", gMean, sep=":"))
  }
}
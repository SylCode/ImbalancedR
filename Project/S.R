library(SDR)
test = read.keel("DataSets/wisconsin-5-fold (IR 1.86)/wisconsin-5-1tst.dat")
mtest <- t(simplify2array(test$data))
n = ncol(mtest)
m = nrow(mtest)
test.classnames = test$class_names
test.features = test$atributeNames[-n]
test.vars = mtest[ , -n]
#test.matr <- as.matrix(test.vars)
colnames(test.vars) <- test.features
pc <- princomp(test.vars,cor=TRUE, scores = TRUE)
summary(pc)
plot(pc, type="lines")
biplot(pc)
library(rgl)
plot3d(pc$scores[,-n], col=test.vars)
#as.dist(test.vars)
#(mds <- cmdscale(test.vars))
set.seed(42)
cl <- kmeans(mtest[,-n],2)
test$cluster <- as.factor(cl$cluster)
plot3d(pc$scores[,-n], col=test$cluster)
source("checkDataSet.R")
result <- checkDataSet(mtest)
print(result)
<<<<<<< HEAD
source("iOver.R")
i <- iOver(mtest, FALSE, TRUE, TRUE, TRUE)
source("iUnder.R")
i <- iUnder(mtest, FALSE, TRUE, TRUE, TRUE)
print(i)
i<-cbind(i$X,i$Y)
resultNew <- checkDataSet(i)
print(resultNew)
=======
source("iOversampling.R")
i <- iOversampling(mtest, 1)
print(i)
>>>>>>> 130232650f87d5d7f219215a55b10faae1483146

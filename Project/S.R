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

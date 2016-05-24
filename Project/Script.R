library(SDR);
training = read.keel("DataSets/yeast5-5-fold (IR 32.73)/yeast5-5-1tra.dat");
test = read.keel("DataSets/yeast5-5-fold (IR 32.73)/yeast5-5-1tst.dat");
names(training)
print(training)
library(unbalanced)
# konwersja wczytanych danych do macierzy
mtrain <- t(simplify2array(training$data))
mtest <- t(simplify2array(test$data))
n <- ncol(mtrain)
# wyciagniecie z danych klas
train.input <- mtrain[ , -n]
train.output <- mtrain[ , n]
test.output <- mtest[ , n]
test.input <- mtest[ , -n]
# klasa mniejszosciowa jako wynik pozytywny
train.output <- (train.output -1)*(-1)
test.output <- (test.output -1)*(-1)
data <- ubOver(X = train.input, Y = train.output) # ubUnder dziala podobnie
library(C50)
train.output = as.factor(data$Y)
train.input = as.data.frame(data$X)
# model drzewa
treemodel <- C5.0(x = train.input, y = train.output)
summary(treemodel)
test.output <- as.factor(test.output)
test.input <- as.data.frame(test.input)
# predykcja
result <- predict(treemodel, test.input)


source("InitTest.R")
source("Execute_C50.R")
source ("fileLooper.R")
genFile="DataSets/wisconsin-5-fold (IR 1.86)/wisconsin-5-"
#genFile = "DataSets/ddcup-rootkit-imap_vs_back-5-fold (IR 100.14)/kddcup-rootkit-imap_vs_back-5-"
params = c(TRUE,FALSE,FALSE,FALSE)
go(genFile, params)


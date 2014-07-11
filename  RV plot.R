
Name<-"VonMises.csv"

setwd("/Users/student/Documents/Bats/")
HR<-read.csv(Name)

hist(HR[which(HR[,1]!=0),1])
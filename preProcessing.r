library(ggplot2)
library(plyr)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)

ggplot(data=Maths, aes(Maths$absences)) + geom_histogram(breaks=seq(0, 80, by =5),col="white", fill="darkblue")
count(Maths,"Result")

#Correlation of G1 with G3

G <- data.frame(Maths$G1,Maths$G2 ,Maths$G3)
correlation <- as.matrix(cor(G))

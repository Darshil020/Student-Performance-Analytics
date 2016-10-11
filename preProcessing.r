library(ggplot2)
library(plyr)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)

ggplot(data=Maths, aes(Maths$absences)) + geom_histogram(breaks=seq(0, 80, by =5),col="white", fill="darkblue")
count(Maths,"Result")

#Correlation of G1 with G3

library(dplyr)
library(reshape2)

setwd("C:/Users/akash/Documents/GitHub/StudentPerformancePrediction")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)

d1 <- data.frame(Maths$G1, Maths$G3)
d1_cor <- as.matrix(cor(d1))
d1_cor_melt <- arrange(melt(d1_cor), -abs(value))
d1_cor_melt

#Correlation of G2 with G3

setwd("C:/Users/akash/Documents/GitHub/StudentPerformancePrediction")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)

d2 <- data.frame(Maths$G2, Maths$G3)
d2_cor <- as.matrix(cor(d2))
d2_cor_melt <- arrange(melt(d2_cor), -abs(value))
d2_cor_melt

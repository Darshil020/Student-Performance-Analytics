library(ggplot2)
library(plyr)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)

ggplot(data=Maths, aes(Maths$absences)) + geom_histogram(breaks=seq(0, 80, by =5),col="white", fill="darkblue")
count(Maths,"Result")

#Correlation of G1 with G3

<<<<<<< HEAD
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


#Correlation of G3- Maths and G3-Por
setwd("C:/Users/akash/Documents/GitHub/StudentPerformancePrediction")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)
Por=read.table("student-por.csv",sep=";",header=TRUE)

MP <- data.frame(Maths$G3, Por$G3)
MP_cor <- as.matrix(cor(MP))
MP_cor_melt <- arrange(melt(d2_cor), -abs(value))
MP_cor_melt

#Boxplots

par(mfrow=c(1,2))
boxplot( Maths$G3, data = Maths,col="yellow",main = "Maths", ylab = "G3 Score")
boxplot(Por$G3, data = Por,col = "green", main = "Portugese", ylab = "G3 Score")

G <- data.frame(Maths$G1,Maths$G2 ,Maths$G3)
correlation <- as.matrix(cor(G))


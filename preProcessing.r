library(ggplot2)
library(plyr)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)

ggplot(data=Maths, aes(Maths$absences)) + geom_histogram(breaks=seq(0, 80, by =5),col="white", fill="darkblue")
count(Maths,"Result")

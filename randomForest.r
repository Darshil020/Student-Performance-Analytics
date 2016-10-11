library(randomForest)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)
#Por=read.table("student-por.csv",sep=";",header=TRUE)

head(Maths)
#converting the result into catagorical
nested_ifelse <- function(x)
  ifelse (x < 10,"fail",ifelse(x < 12, "D", ifelse(x < 14,"C",ifelse(x < 16, "B","A")) ))
Result <- nested_ifelse(Maths$G3)  #converting G3 to categorical Result

Maths <- data.frame(Maths,Result) #appending the grades Result
Maths = Maths[,-33]        #remove the G3

set.seed(2)
train = sample(1:nrow(Maths),nrow(Maths)/2)
test = -train
training_data = Maths[train,]
testing_data = Maths[test,]
testing_result = Result[test]

#RandomForest 
rfm = randomForest(Result~.,training_data)
rmf_pred = predict(rfm,testing_data)
mean(rmf_pred == testing_result) #66.66%
table(rmf_pred,testing_result)

library(rpart)
library(rpart.plot)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)
Por=read.table("student-por.csv",sep=";",header=TRUE)

head(Maths)
#converting the result into catagorical
nested_ifelse <- function(x)
  ifelse (x < 11,"C",ifelse(x < 16, "B", "A" ))
Result <- nested_ifelse(Maths$G3)  #converting G3 to categorical Result

Maths <- data.frame(Maths,Result) #appending the grades Result
Maths = Maths[,-33]        #remove the G3
#Maths = Maths[,-31:-32]
#Maths_factor <- lapply(Maths,as.factor)

set.seed(2)
train = sample(1:nrow(Maths),nrow(Maths)/2)
test = -train
training_data = Maths[train,]
testing_data = Maths[test,]
testing_result = Result[test]

#Decision tree rpart
tree_model = rpart(Result~.,training_data,method = 'class')
rpart.plot(tree_model)
text(tree_model,pretty = 0)
tree_pred = predict(tree_model,testing_data,type = 'class')
mean(tree_pred != testing_result) #8.58%


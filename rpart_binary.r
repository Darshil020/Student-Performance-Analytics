library(rpart)
library(rpart.plot)
setwd("C:/Users/User/documents/GitHub/StudentPerformancePrediction")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)
Por=read.table("student-por.csv",sep=";",header=TRUE)

head(Maths)
#converting the result into catagorical
nested_ifelse <- function(x)
  ifelse (x < 10,"fail","Pass")
Result <- nested_ifelse(Maths$G3)  #converting G3 to binary (pass-fail)

Maths <- data.frame(Maths,Result) #appending the grades Result
Maths = Maths[,-31:-33]        #remove G1,G3 and G3
head(Maths)

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
plotcp(tree_model)
tree_pred = predict(tree_model,testing_data,type = 'class')
mean(tree_pred == testing_result) #Accuracy: 64.64%
table(tree_pred,testing_result)

#prunning the tree
pfit<- prune(tree_model,cp=tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
rpart.plot(pfit)
plotcp(pfit)
tree_pred = predict(pfit,testing_data,type = 'class')
mean(tree_pred == testing_result) #Accuracy: 71.71%
table(tree_pred,testing_result)

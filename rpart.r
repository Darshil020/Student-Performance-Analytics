library(rpart)
library(rpart.plot)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)
Por=read.table("student-por.csv",sep=";",header=TRUE)

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

#Decision tree rpart
tree_model = rpart(Result~famrel+absences+G1+G2,training_data,method = 'class',control = rpart.control(cp=0.01))
rpart.plot(tree_model)
plotcp(tree_model)
tree_pred = predict(tree_model,testing_data,type = 'class')
mean(tree_pred == testing_result) #81.81%
table(tree_pred,testing_result)

#pruning the tree
pfit<- prune(tree_model,cp=tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"])
rpart.plot(pfit)
plotcp(pfit)
tree_pred = predict(pfit,testing_data,type = 'class')
mean(tree_pred == testing_result) #80.80%
table(tree_pred,testing_result)

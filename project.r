library(tree)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)

head(Maths)
#converting the result into catagorical
nested_ifelse <- function(x)
  ifelse (x < 10,"fail",ifelse(x < 12, "D", ifelse(x < 14,"C",ifelse(x < 16, "B","A")) ))
Result <- nested_ifelse(Maths$G3)  #converting G3 to categorical Result

Maths <- data.frame(Maths,Result) #appending the grades Result
Maths = Maths[,-33]        #remove the G3
#Maths = Maths[,-31:-32]

set.seed(2)
train = sample(1:nrow(Maths),nrow(Maths)/2)
test = -train
training_data = Maths[train,]
testing_data = Maths[test,]
testing_result = Result[test]

#Decision tree
tree_model = tree(Result~.,training_data)
plot(tree_model)
text(tree_model,pretty = 0)
tree_pred = predict(tree_model,testing_data,type = 'class')
mean(tree_pred == testing_result) #71.71%

#Pruning the tree
set.seed(3)
cv_tree = cv.tree(tree_model,FUN = prune.misclass)
plot(cv_tree$size,cv_tree$dev,type="b")

#prunned model
pruned_model = prune.misclass(tree_model,best = 6)
plot(pruned_model)
text(pruned_model,pretty=0)

#check performance of the pruned model
tree_pred = predict(pruned_model,testing_data,type="class")
mean(tree_pred == testing_result) #76.76%

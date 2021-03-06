library(tree)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)
#Por=read.table("student-por.csv",sep=";",header=TRUE)

head(Maths)
#converting the result into catagorical
nested_ifelse <- function(x)
  ifelse (x < 11,"C",ifelse(x < 16, "B", "A" ))
Result <- nested_ifelse(Maths$G3)  #converting G3 to categorical Result

#converting absenses into categorical
nested_ifelse <- function(x)
  ifelse (x < 5,"Low",ifelse(x < 8, "Medium", "High" ))
absences1 <- nested_ifelse(Maths$absences)  #converting absents to categorical Result

Maths <- data.frame(Maths,absences1,Result) #appending the grades Result
Maths = Maths[,-33]        #remove G3
Maths = Maths[,-30:-32]   #remove G2,G1
#Maths_factor <- lapply(Maths,as.factor)

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
mean(tree_pred != testing_result) #12.62%

#Pruning the tree
set.seed(3)
cv_tree = cv.tree(tree_model,FUN = prune.misclass)
plot(cv_tree$size,cv_tree$dev,type="b")

#prunned model
pruned_model = prune.misclass(tree_model,best = 2)
plot(pruned_model)
text(pruned_model,pretty=0)

#check performance of the pruned model
tree_pred = predict(pruned_model,testing_data,type="class")
mean(tree_pred != testing_result) #8%

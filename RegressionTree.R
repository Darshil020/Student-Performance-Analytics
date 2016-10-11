library(tree)
setwd("D:/Business Analytics/Business Analytics using R/Project Data")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)

head(Maths)
Maths$G3 = as.factor(Maths$G3)
set.seed(2)
train = sample(1:nrow(Maths),nrow(Maths)/2)
test = -train
training_data = Maths[train,]
testing_data = Maths[test,]

#Decision tree
tree_model = tree(G3~.,training_data)
plot(tree_model)
text(tree_model,pretty = 0)


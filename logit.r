install.packages("ISLR")
library(ISLR)
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

#LogisticRegression
result_model <- glm(Result~.,data = training_data, family = binomial)  
summary(result_model)
result_model <- glm(Result~ age+Mjob+failures+schoolsup+nursery+goout+absences,data = training_data, family = binomial)  #only significant variable used
predict_result <- predict(result_model,testing_data,type = "response")
model_pred_result = rep("Fail",198)
model_pred_result[predict_result>0.5] = "Pass"

#confusionmatrix
table(model_pred_result,testing_result)
mean(model_pred_result != testing_result)

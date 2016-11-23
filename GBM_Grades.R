library(gbm)
setwd("C:/Users/akash/Documents/GitHub/StudentPerformancePrediction")
Maths=read.table("student-mat.csv",sep=";",header=TRUE)
head(Maths)

#converting the result into catagorical

nested_ifelse <- function(x) 
  ifelse (x < 10,"fail",ifelse(x < 12, "D", ifelse(x < 14,"C",ifelse(x < 16, "B","A")) ))

Result <- nested_ifelse(Maths$G3)  #converting G3 to categorical Result
Result
Maths <- data.frame(Maths,Result) #appending the grades Result
Maths = Maths[,-33]        #remove the G3
#Maths = Maths[,-30]


#Dividing the train and test data
set.seed(2)
train = sample(1:nrow(Maths),nrow(Maths)/2)
test = -train
training_data = Maths[train,]
testing_data = Maths[test,]
testing_result = Result[test]

#GBM
fit<- gbm(Result~.,data=training_data,distribution = "multinomial",n.trees = 500,shrinkage = 0.05,interaction.depth = 3,bag.fraction = 0.5,keep.data = FALSE,cv.folds = 5)
nTrees <- gbm.perf(fit)
print(nTrees)
print(summary(fit))

test_prediction <- predict(fit,testing_data,n.trees = nTrees,type = "response")
test_answer <- round(test_prediction)

test_answer<-as.data.frame(test_answer)

test_answer$Answer=NA
colnames(test_answer)
for(colname in colnames(test_answer))({
  coldata=test_answer[,colname]
  test_answer$Answer[which(coldata==1)]=colname
})
test_answer$Answer<-gsub(".41","",test_answer$Answer)
test_answer$Answer<- as.factor(test_answer$Answer)
mean(test_answer$Answer == testing_result,na.rm = FALSE)
table(test_answer$Answer,testing_result) #887.38% 15,29,27,41,63

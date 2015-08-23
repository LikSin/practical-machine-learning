#Practical Machine Learning Quiz

#Quiz 2
#Q1 - method to create 50% training and 50% test sets
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#q2 - 
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)
summary(training$Superplasticizer)

#q3 - Calculate the number of principal components needed to capture 80% of the variance. How many are there?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

str(training)
trainingselect=training[,grepl("^IL",colnames(training))]
str(trainingselect)
preObj=preProcess(trainingselect, method="pca",thresh=0.8)
preObj

#q4 - compare accuracy of PCA and glm method
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingselect=training[,grepl("^IL",colnames(training))]
str(trainingselect)
model1=train(training$diagnosis~.,method="glm",data=trainingselect)
predictions <- predict(model1, newdata=testing)
confusionMatrix(predictions, testing$diagnosis)

model2=train(training$diagnosis~., method="glm", preProcess="pca", data=trainingselect, trControl=trainControl(preProcOptions=list(thresh=0.8)))
predictions2 <- predict(model2, newdata=testing)
confusionMatrix(predictions2, testing$diagnosis)


#Quiz 3

#q1 - plot classification tree
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
testing=subset(segmentationOriginal, segmentationOriginal$Case=="Test")
training=subset(segmentationOriginal, segmentationOriginal$Case=="Train")
set.seed(125)
model=train(Class~. ,method="rpart", data=training)
print(model$finalModel)
plot(model$finalModel, uniform=TRUE, main="Classification Tree")
text(model$finalModel, use.n = TRUE, all = TRUE, cex=.8)
#alternative
library(rattle)
fancyRpartPlot(model$finalModel)

#q3 - plot classification tree
library(pgmm)
data(olive)
olive = olive[,-1]
library(caret)
model=train(Area~. ,method="rpart", data=olive)
print(model$finalModel)
library(rattle)
fancyRpartPlot(model$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
newdata
predict(model,newdata)


#q4 - predict using logistic regression model(glm) method
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
model=train(chd~age+alcohol+obesity+tobacco+typea+ldl, method="glm", data=trainSA, family="binomial")
model
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
predictTrain=predict(model,trainSA)
predictTest=predict(model,testSA)
missClass(trainSA$chd,predictTrain)
missClass(testSA$chd,predictTest)


#q5 - random forest prediction method
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
set.seed(33833)

library(caret)
model=train(y~., method="rf", data=vowel.train)
model
varImp(model)


##assignment submission
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(as.character(predval))

library(caret)
library(kernlab)
data("spam");
inTrain<-createDataPartition(y=spam$type, p=0.75, list=FALSE);
training<- spam[inTrain,];
testing<-spam[-inTrain,];
dim(training);
set.seed(32343)

#install.packages("e1071")
library(e1071)
modelFit=train(type ~., data=training, method="glm")
modelFit
modelFit$finalModel

predictions<- predict(modelFit,newdata=testing)
predictions
confusionMatrix(predictions,testing$type)

#training folds
set.seed(32323)

folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

#test folds
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = FALSE)
sapply(folds, length)
folds[[1]][1:10]

#resampling
folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds, length)
folds[[1]][1:10]

#timeslices
tme<-1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]


# plots
#install.packages("ISLR")
library(ISLR)
library(ggplot2)
data("Wage")
summary(Wage)

inTrain<-createDataPartition(y=Wage$wage, p=0.7, list=FALSE);
training<- Wage[inTrain,];
testing<-Wage[-inTrain,];
dim(training)
dim(testing)
featurePlot(x=training[,c("age", "education", "jobclass")], y=training$wage, plot="pairs")


# pre processing
inTrain<-createDataPartition(y=spam$type, p=0.75, list=FALSE);
training<- spam[inTrain,];
testing<-spam[-inTrain,];
hist(training$capitalAve, main="", xlab="ave. capital")
mean(training$capitalAve)
sd(training$capitalAve)
trainCapitalAve <- training$capitalAve
trainCapitalAveS <- (trainCapitalAve - mean(trainCapitalAve))/sd(trainCapitalAve)
mean(trainCapitalAveS)
sd(trainCapitalAveS)
testCapitalAveS <- (testing$capitalAve - mean(trainCapitalAve))/sd(trainCapitalAve)
mean(testCapitalAveS)
sd(testCapitalAveS)

# use preprocessing when data in a variable is highly skewed and/or has missing values
# - apply same scaling parameters to both training and testing data sets
# - caret packages can do it behind the scenes

modelFit<-train(training$type ~ ., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit, testing))

# Quiz 2
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(AlzheimerDisease)

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

#install.packages("Hmisc")
library(Hmisc)
dim(mixtures)
plot(mixtures$CompressiveStrength)
plot(mixtures$CompressiveStrength, col=cut2(mixtures$Cement, g=5))
plot(mixtures$CompressiveStrength, col=cut2(mixtures$BlastFurnaceSlag, g=5))
plot(mixtures$CompressiveStrength, col=cut2(mixtures$FlyAsh, g=5))
plot(mixtures$CompressiveStrength, col=cut2(mixtures$Water, g=5))
plot(mixtures$CompressiveStrength, col=cut2(mixtures$Superplasticizer, g=5))
plot(mixtures$CompressiveStrength, col=cut2(mixtures$CoarseAggregate, g=5))
plot(mixtures$CompressiveStrength, col=cut2(mixtures$FineAggregate, g=5))
plot(mixtures$CompressiveStrength, col=cut2(mixtures$Age, g=5))
summary(mixtures)

hist(mixtures$Superplasticizer)
hist(mixtures$Superplasticizer)
hist(log(mixtures$Superplasticizer))
hist(log(mixtures$Superplasticizer+1))
hist(mixtures$Superplasticizer^2)
hist(exp(mixtures$Superplasticizer))

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
summary(training)


# IL_11           IL_13           IL_16            IL_17E        IL_1alpha           IL_3       
# Min.   :2.031   Min.   :1.240   Min.   :0.9568   Min.   :1.052   Min.   :-8.517   Min.   :-5.915  
# 1st Qu.:3.706   1st Qu.:1.274   1st Qu.:2.4613   1st Qu.:4.149   1st Qu.:-7.849   1st Qu.:-4.269  
# Median :4.805   Median :1.283   Median :2.9245   Median :4.803   Median :-7.543   Median :-3.963  
# Mean   :4.700   Mean   :1.284   Mean   :2.9146   Mean   :4.905   Mean   :-7.537   Mean   :-3.950  
# 3rd Qu.:5.682   3rd Qu.:1.290   3rd Qu.:3.3514   3rd Qu.:5.631   3rd Qu.:-7.264   3rd Qu.:-3.576  
# Max.   :7.801   Max.   :1.317   Max.   :4.9367   Max.   :8.952   Max.   :-5.952   Max.   :-2.453  
# IL_4             IL_5               IL_6          IL_6_Receptor           IL_7       
# Min.   :0.5306   Min.   :-1.04982   Min.   :-1.53428   Min.   :-0.74560   Min.   :0.5598  
# 1st Qu.:1.4586   1st Qu.:-0.09431   1st Qu.:-0.41275   1st Qu.:-0.14746   1st Qu.:2.1548  
# Median :1.8083   Median : 0.18232   Median :-0.09343   Median : 0.09669   Median :2.9245  
# Mean   :1.7649   Mean   : 0.18759   Mean   :-0.13856   Mean   : 0.07889   Mean   :2.9094  
# 3rd Qu.:2.0794   3rd Qu.: 0.47000   3rd Qu.: 0.18569   3rd Qu.: 0.27297   3rd Qu.:3.7055  
# Max.   :3.0445   Max.   : 1.16315   Max.   : 1.81380   Max.   : 0.77048   Max.   :5.7056  
# IL_8 

dim(training)
names(training)
58:69
preProc <- preProcess(training[,58:69], method="pca", thresh = 0.9)
preProc
summary(prcomp(training[,58:69], scale = TRUE))


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
names(predictors)
adData = data.frame(diagnosis,predictors[57:68])
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

model1 <- train(diagnosis ~ .,data = training, method="glm")
model1
confusionMatrix(testing$diagnosis,predict(model1,testing))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Impaired Control
# Impaired        2      20
# Control         9      51
# 
# Accuracy : 0.6463         
# 95% CI : (0.533, 0.7488)
# No Information Rate : 0.8659         
# P-Value [Acc > NIR] : 1.00000 

preProc <- preProcess(training[,-1], method="pca", thresh = 0.8)
trainPC<-predict(preProc, training)
model2 <- train(diagnosis ~ ., data=trainPC, method="glm")
model2
testPC <- predict(preProc,testing[,-1])
confusionMatrix(testing$diagnosis,predict(model2,testPC))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Impaired Control
# Impaired        3      19
# Control         4      56
# 
# Accuracy : 0.7195          
# 95% CI : (0.6094, 0.8132)
# No Information Rate : 0.9146          
# P-Value [Acc > NIR] : 1.000000        

# quiz 3
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
sessionInfo()
library(ElemStatLearn)
#install.packages("ElemStatLearn")
library(ElemStatLearn)
#install.packages("pgmm")
library(pgmm)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
summary(segmentationOriginal)
#1. Subset the data to a training set and testing set based on the Case variable in the data set.
training <- segmentationOriginal[which(segmentationOriginal$Case=="Train"),]
testing <- segmentationOriginal[which(segmentationOriginal$Case=="Test"),]

#2. Set the seed to 125 and fit a CART model with the rpart method 
# using all predictor variables and default caret settings.
set.seed(125)
modelFit <- train(Class ~., method="rpart", data = training)
print(modelFit$finalModel)
plot (modelFit$finalModel)
rpart.plot(modelFit, box.palette="RdBu", shadow.col="gray", nn=TRUE)
install.packages("rattle")
library(rattle)
fancyRpartPlot(modelFit$finalModel)
#3. In the final model what would be the final model prediction for cases with the following variable values:
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
# WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
# PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2
# impossible to predict

# If K is small in a K-fold cross validation is the bias in the estimate of 
# out-of-sample (test set) accuracy smaller or bigger? If K is small is the 
# variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. 
# Is K large or small in leave one out cross validation?

# small K: larger bias, smaller variance
# LOOCV: same size as sample

library(pgmm)
data(olive)
olive = olive[,-1]
modelFit <- train(as.factor(Area) ~., method="rpart", data = olive)
newdata = as.data.frame(t(colMeans(olive)))
predictions<- predict(modelFit,newdata=newdata)
predictions
fancyRpartPlot(modelFit$finalModel)

modelFit2 <- train(as.factor(Area) ~., method="rpart", data = olive)
fancyRpartPlot(modelFit2$finalModel)
predictions2<- predict(modelFit,newdata=newdata)

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# Then set the seed to 13234 and 
set.seed(13234)
# fit a logistic regression model (method="glm", be sure to specify family="binomial") 
# with Coronary Heart Disease (chd) as the outcome and age at onset, 
# current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior,
# and low density lipoprotein cholesterol as predictors.
print(trainSA)
# sbp tobacco   ldl adiposity famhist typea obesity alcohol age chd
# 238 176    5.76  4.89     26.10 Present    46   27.30   19.44  57   0
# 114 174    0.00  8.46     35.10 Present    35   25.27    0.00  61   1

train_control = trainControl(method="cv", number=5)
modelFit = train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, method="glm", family=binomial, trControl=train_control)
PredTrain = predict(modelFit, newdata=trainSA, type="raw") 

# Calculate the misclassification rate for your model using this function and 
# a prediction on the "response" scale:
	
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
	
# What is the misclassification rate on the training set? 
missClass(trainSA$chd, predict(modelFit, newdata=trainSA))
# What is the misclassification rate on the test set?
missClass(testSA$chd, predict(modelFit, newdata=testSA))
# 
# > missClass(trainSA$chd, predict(modelFit, newdata=trainSA))
# [1] 0.2727273
# > missClass(testSA$chd, predict(modelFit, newdata=testSA))
# [1] 0.3116883

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
set.seed(33833)
install.packages("randomForest")
library(randomForest)
modelFit<-randomForest(y ~., data = vowel.train)
varImp(modelFit)

modelFit2<-train(y ~., data = vowel.train, method="rf")
varImp(modelFit2)

# quiz 4
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
set.seed(33833)
modelrf<-train(y ~., data = vowel.train, method="rf")
modelgbm<-train(y ~., data = vowel.train, method="gbm", verbose=FALSE)

confusionMatrix(vowel.test$y,predict(modelrf,vowel.test))
confusionMatrix(vowel.test$y,predict(modelgbm,vowel.test))

# combine both models
predrf<-predict(modelrf,vowel.test)
predgbm<-predict(modelgbm,vowel.test)

predAgr<- data.frame(predrf, predgbm, y=vowel.test$y)
modelAgr<-train(y ~., method="gam", data=predAgr)
combPred<-predict(modelAgr, predAgr)
confusionMatrix(vowel.test$y,combPred)

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
m1<-train(diagnosis ~., data=training, method="rf", verbose=FALSE)
m2<-train(diagnosis ~., data=training, method="gbm", verbose=FALSE)
m3<-train(diagnosis ~., data=training, method="lda", verbose=FALSE)
p1<-predict(m1, testing)
p2<-predict(m2, testing)
p3<-predict(m3, testing)
predAll<-data.frame(p1, p2, p3, diagnosis=testing$diagnosis)
mCombo<-train(diagnosis ~., data=predAll, method="rf")
confusionMatrix(testing$diagnosis, predict(mCombo, testing)) # 0.792

confusionMatrix(testing$diagnosis, predict(m1, testing)) # 0.792
confusionMatrix(testing$diagnosis, predict(m2, testing)) # 0.781
confusionMatrix(testing$diagnosis, predict(m3, testing)) # 0.781

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

modelLasso<-train(CompressiveStrength ~., data=training, method="lasso")
plot(modelLasso$finalModel, xvar="penalty", use.color=TRUE)

library(lubridate) # For year() function below
dat = read.csv("../data/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)
?bats
print(tstrain)
fit<-bats(tstrain)
plot(forecast(fit))
print(forecast(testing$date, model=fit))


set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
modelFit=svm(CompressiveStrength ~., data=training)
modelFit
modelFit$finalModel
pred<-predict(modelFit, testing)
RMSE(testing$CompressiveStrength, pred)

# c8_project

setwd("C:/Users/paulo.faleiros/Google Drive/Coursera/courses/JHU-DS/code")
trainingRdat = read.csv("../data/pml-training.csv", stringsAsFactors = FALSE)
validation = read.csv("../data/pml-testing.csv", stringsAsFactors = FALSE)
#head(trainingRdat)
library(ggplot2)
library(ISLR)
library(caret)
library(kernlab)
library(e1071)
featurePlot(x=trainingRdat[,c(8:11, 37:45)], y=trainingRdat$classe, plot="pairs")
library(Hmisc)

plot(trainingRdat$classe, log(trainingRdat$roll_belt+30))
str(trainingRdat$roll_belt)
summary(trainingRdat$roll_belt)
summary(log(trainingRdat$roll_belt+30))


set.seed(20190114)
pmlData = createDataPartition(y=trainingRdat$classe, p=0.7, list=FALSE)
training = trainingRdat[pmlData,]
testing = trainingRdat[-pmlData,]
training[training=="" || training==" " || training=="NA" || training=="#DIV/0!"]<-NA
testing[testing=="" || testing==" " || testing=="NA" || testing=="#DIV/0!"]<-NA
validation[validation=="" || validation=="NA" || validation=="#DIV/0!"]<-NA
sapply(training, function(x) sum(is.na(as.numeric(x))))
sapply(testing, function(x) sum(is.na(x)))
sapply(validation, function(x) sum(is.na(x)))
names(x[x==dim(training)[1]])
13454/13737

rowSums(is.na(training))
sapply(training, function(x) summary(x))

qplot(classe, user_name, data=training)
sapply(training, function(x) class(x))


library(foreach)
a<-foreach (i=names(x[x==dim(training)[1]])) %do% training[1,i]

t2<-training
x<-sapply(training, function(x) class(x))
t2<-foreach (i=x[x=="character"]) %do% as.factor(training[,x[i]])

training1<-training[,c(8:11, 37:45,160)]
#preProc <- preProcess(training1[,-14], method="pca", thresh = 0.8)
#trainPC<-predict(preProc, training1)
model2 <- train(classe ~ ., data=training1, method="rf")
model2$finalModel


testing1<-testing[,c(8:11, 37:45,160)]
sum(is.na(predict(model2, testing1)))
sum(is.na(testing1$classe))

RMSE(testing1$classe, predict(model2, testing1))
pred<-predict(model2, testing1)
confusionMatrix(pred, as.factor(testing1$classe))
summary(as.factor(testing1$classe))
# all:
# 	randomForest(x = x, y = y, mtry = param$mtry) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 7
# 
# OOB estimate of  error rate: 7.61%
# Confusion matrix:
# 	A    B    C    D    E class.error
# A 3648   74   87   71   26  0.06605223
# B   81 2447  104   24    2  0.07938299
# C  111  129 2073   81    2  0.13480801
# D  102   21   57 2063    9  0.08392540
# E   45    9    7    4 2460  0.02574257


# TO-DO
# handle missing values in the remaining variables
# apply preprocessing
# apply boosting models


modelFit1<-train(as.factor(classe) ~. , data=training1, method="rf")

validation<-validation[,c(8:11, 37:45)]
validation<-data.frame(validation, classe=rep("",20))
predictions<-predict(model2, validation)
print(predictions)
# > print(predictions)
# [1] B A B A A E D B A A B C B A E E A B B B

training[training=="" || training=="NA" || training=="#DIV/0!"]<-NA
summary(training$kurtosis_roll_belt)
mean(training[which(training$new_window=="yes"),training$kurtosis_roll_belt], na.rm=TRUE)
training$kurtosis_roll_belt<-as.numeric(training$kurtosis_roll_belt)


x<- matrix(1:10, ncol=2)
x[c(1,3,7)] <- NA
print(x)
head(impute(training))

library(e1071)

qplot(classe, roll_belt, data=training1)
qplot(classe, pitch_belt, data=training1)
qplot(classe, yaw_belt, data=training1)
qplot(classe, total_accel_belt, data=training1)

qplot(classe, pitch_belt, data=training1)
plot(trainingRdat$classe, col=cut2(trainingRdat$, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_roll_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_picth_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_yaw_belt, g=5))
plot(trainingRdat$classe, col=cut2(log(trainingRdat$skewness_roll_belt^2), g=5), na.omit=TRUE)
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_roll_belt.1, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_yaw_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_roll_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_picth_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_yaw_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_roll_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_pitch_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_yaw_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_roll_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_pitch_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_yaw_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_total_accel_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_roll_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_roll_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_roll_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_pitch_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_pitch_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_pitch_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_yaw_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_yaw_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_yaw_belt, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_belt_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_belt_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_belt_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_belt_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_belt_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_belt_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_belt_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_belt_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_belt_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$pitch_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$total_accel_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_accel_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_pitch_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_pitch_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_pitch_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_arm_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_arm_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_arm_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_arm_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_arm_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_arm_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_arm_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_arm_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_arm_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_picth_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_pitch_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_picth_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_pitch_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_roll_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_pitch_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_yaw_arm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$pitch_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_picth_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_pitch_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_picth_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_pitch_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_pitch_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$total_accel_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_accel_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_roll_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_pitch_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_pitch_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_pitch_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_yaw_dumbbell, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_dumbbell_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_dumbbell_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_dumbbell_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_dumbbell_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_dumbbell_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_dumbbell_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_dumbbell_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_dumbbell_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_dumbbell_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$pitch_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_picth_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$kurtosis_yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_pitch_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$skewness_yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_picth_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$max_yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_pitch_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$min_yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_pitch_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$amplitude_yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$total_accel_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_accel_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_roll_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_pitch_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_pitch_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_pitch_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$avg_yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$stddev_yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$var_yaw_forearm, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_forearm_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_forearm_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$gyros_forearm_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_forearm_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_forearm_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$accel_forearm_z, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_forearm_x, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_forearm_y, g=5))
plot(trainingRdat$classe, col=cut2(trainingRdat$magnet_forearm_z, g=5))


rm(list=ls())

library(randomForest)
library(gbm)
library(caret)
library(Ecdat)
library(e1071)

#-------------------------------data tampering---------------------------------------------------

#read data from the provided file
import.data <- read.csv("training_data.csv", header = TRUE)
import.data$mode = as.factor(import.data$mode)
#display data in condensed form
str(import.data)
#set seed ensures us that we can reproduce the same random numbers, this is good while writing code.
#when the code is done this is redundant.
#set.seed(945)

#sample the data into two independendt randomized data sets,
#using the distribution 0.7 for test data and 0.3 for the trainingdata.
index = sample(2,nrow(import.data),replace = TRUE, prob=c(.8,.2))
train_data <- import.data[index==1,]
test_data <- import.data[index==2,]

#-------------we want to make a sweep in order to find the best params for the gbm--------------------------

grid <- expand.grid(.n.trees = seq(200,500,by=100), .interaction.depth=seq(1,5,by=2), 
                   .shrinkage=seq(.01,.09,by=.03), .n.minobsinnode=seq(1,5,by=2))
control <- trainControl(method="CV",number = 10)

#again seting the seed to a diffrent value for the RNG and then procceding to train our model.
#the ouputs of our gbm model will give the parameters for our final model.
#set.seed(24)
boost_model_train <- train(label~.,data=train_data,method='gbm',trControl=control,tuneGrid=grid)
boost_model_train

#--------------Construct model final model with the params given by the sweep--------------------------------

#we transform the label column to binary 0 == dislike, 1 == like.
#we create the final model uding it the training data with parms given by the gbm method
train_data$label=ifelse(train_data$label=="dislike",0,1)
final_model <- gbm(label~., distribution = 'bernoulli',data=train_data,n.trees = boost_model_train$bestTune$n.trees
                   ,interaction.depth = boost_model_train$bestTune$interaction.depth,
                   shrinkage=boost_model_train$bestTune$shrinkage,
                   n.minobsinnode = boost_model_train$bestTune$n.minobsinnode)

#ploting the weight of each feature, also gives the table with relative "worth"
summary(final_model)

#testing the model using the final model and the partioned test data from the begining
classify_test <- predict(final_model,newdata = test_data,type = 'response', n.trees = boost_model_train$bestTune$n.trees)
classify <- ifelse(classify_test<0.5,'dislike','like')

#saving the TF TT , FT FF values in t
t <- table(classify,test_data$label)

#calculating the accuracy of the model
accuracy <- (t[1,1]+t[2,2])/sum(t)


#---------------use model to predict the songs---------------------


#importing songs to classify and using our classifier to the data
songs <- read.csv("songs_to_classify.csv", header = TRUE)

gbm.label.stc <- predict(gbm.label,newdata = songs, type = 'response', n.trees = 200)
gbm.class.stc <- ifelse(gbm.label.stc<0.5,'dislike','like')

#converting the data to binary for submission
gbm.binary <- ifelse(gbm.class.stc == "dislike",0,1)




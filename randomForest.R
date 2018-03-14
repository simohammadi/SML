#install.packages("party")
#install.packages("randomForest")
library(party)
library(rpart)
library(randomForest)
data <- read.csv("training_data_tampered.csv", header = TRUE)
toclassifydata <- read.csv("songs_to_classify.csv", header = TRUE)
#set.seed(1);
train <- sample(x=1:nrow(data), size = 400, replace = FALSE)
data.train <- data[train,]
data.test <- data[-train,]
N = 500
numTrees = 100

data.train$label = as.factor(data.train$label)
data.train$mode = as.factor(data.train$mode)
data.test$label = as.factor(data.test$label)
data.test$mode = as.factor(data.test$mode)
#toclassifydata$mode = as.factor(toclassifydata$mode)
rf.fit = randomForest(formula = label ~ danceability + energy + key + loudness + mode +
                    speechiness + acousticness + instrumentalness + liveness + valence +
                    tempo + duration + time_signature, data = data.train, nodesize = 2, ntree = numTrees)
rf.pred = predict(rf.fit, newdata=data.test, predict.all=TRUE)

test.error.rf = rep(0,numTrees)
yp = rep(0,numTrees) # Aggregated prediction
result = rep(1,numTrees)
 
rf.pred$individual[,numTrees]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
yp <- rf.pred$individual[]


############################################################
##EITHER:

for(i in 1:B){
  temp <- getmode(as.numeric(yp[i,]))
  if(temp == data.test$label[i]){
    result[i] = 0
  }
}
sum(result==0)
##OR:
#If we want to try the optimal number of trees:(Change numTrees=1000)at start
#for(i in 1:numTrees){
#  result = rep(1,B)
#  for(j in 1:B){
#    
#    temp = getmode(as.numeric(yp[j,1:i])) # This is the prediction for the i:th tree
#    if(temp == data.test$label[j]){
#      result[j] = 0
#    }
#  }
#  test.error.rf[i] = sum(result==1)
#}
#plot(test.error.rf,pch=19,col="chartreuse3",xlab="Number of trees",ylab="Test error")
#plot(test.error.rf, xlab="Number of trees",ylab="Test error")  
  
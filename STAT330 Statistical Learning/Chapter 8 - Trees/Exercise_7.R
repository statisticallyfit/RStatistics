setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 8 - Trees")

library(tree)
library(ISLR)
library(MASS)
#library(ggplot2)
library(gridExtra)
library(randomForest)

data("Boston")


## RANDOM FOREST -----------------------------------------------------------------------------

set.seed(1)

N <- nrow(Boston)

trainIndices <- sample(1:N, size=N/2)
X.train <- Boston[trainIndices, -14] # 14 var medv = response
X.test <- Boston[-trainIndices, -14]
Y.train <- Boston[trainIndices, 14]
Y.test <- Boston[-trainIndices, 14]

p = dim(Boston)[2]-1; p
p.2 <- p/2
p.sq <- sqrt(p)
numTrees = 500

boston.rf_p <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test, mtry=p, ntree=numTrees)
boston.rf_p.2 <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test, mtry=p.2, ntree=numTrees)
boston.rf_p.sq <- randomForest(X.train, y=Y.train, xtest=X.test, ytest=Y.test, mtry=p.sq, ntree=numTrees)


df <- data.frame(NumTrees=1:numTrees, P.mse = boston.rf_p$test$mse, P2.mse = boston.rf_p.2$test$mse, 
                 PSQ.mse = boston.rf_p.sq$test$mse)

ggplot(data=df, aes(x=NumTrees)) + geom_line(aes(y=P.mse, colour="green"), size=1) + 
      geom_line(aes(y=P2.mse, colour="red"), size=1) + 
      geom_line(aes(y=PSQ.mse, colour="blue"), size=1) + 
      xlab("Number of Trees") + ylab("Test MSE")

# CONCLUDE: MSE rate is high for NumTree = 1 and decreases with increase in tree size. 
# There must be some strong predictors since the random forest method (lower M = sqrt(p))
# works better than bagging (m = p)

# With this method of calling randomForest(), the below code won't work ... says no forest
# component in the object
# pred.rf <- predict(boston.rf_p, newdata=Boston[-trainIndices,])
# testMSE.boston.rf <- mean((pred.rf - Y.test)^2); testMSE.boston.rf
# 11.66454


### Check importance of each variable (mean decrease of MSE due to splits, for a given predictor
# averaging over all B bootstrap-sampled trees)
# importance(boston.rf)
# varImpPlot(boston.rf)
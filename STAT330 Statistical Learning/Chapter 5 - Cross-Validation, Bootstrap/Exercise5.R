setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 5 - Cross-Validation, Bootstrap/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(MASS)
library(caret)

data("Default")

#Goal: estimate test error of the logistic model using validation set approach


# a) fit model
default.glm <- glm(default ~ income + balance, data=Default, family=binomial)

# b) validation approach

# split sample into training and validation
set.seed(1)


logisticValidationRunner <- function() {
      N = nrow(Default)
      Nsplit = N / 2
      
      # split the data
      trainIndices = sample(1:N, size=Nsplit)
      
      # fit training model
      default.train.glm <- glm(default ~ income + balance, data=Default, family=binomial, 
                               subset=trainIndices)
      
      # predict default on validation, do classification
      validationData <- Default[- trainIndices, ]
      pred.probs.test <- predict(default.train.glm, validationData, type="response")
      pred.label.test <- rep("No", Nsplit)
      pred.label.test[pred.probs.test > 0.5] <- "Yes"
      
      # compute validation set error
      # confusion matrices to check the accuracy calculated is correct
      #confusionMatrix(data=pred.label.test, reference=validationData$default, positive="Yes")
      #table(PredictionTest=pred.label.test, TruthTest=validationData$default)
      
      #mean("Yes" == validationData$default) # random chance
      #accuracy = mean(pred.label.test == validationData$default); accuracy
      validationTestErrorRate = mean(pred.label.test != validationData$default)
      return(validationTestErrorRate)
      
}

# first time: got
# test error rate = 0.0286

# c) repeat above three times, using three different splits of training / validation data.
logisticValidationRunner()
# 0.0236
logisticValidationRunner()
# 0.028
logisticValidationRunner()
# 0.0268


# d) new model test error rate

logisticValidationRunner2 <- function() {
      N = nrow(Default)
      Nsplit = N / 2
      
      # split the data
      trainIndices = sample(1:N, size=Nsplit)
      
      # fit training model
      default.train.glm <- glm(default ~ income + balance + student, 
                               data=Default, family=binomial, subset=trainIndices)
      
      # predict default on validation, do classification
      validationData <- Default[- trainIndices, ]
      pred.probs.test <- predict(default.train.glm, validationData, type="response")
      pred.label.test <- rep("No", Nsplit)
      pred.label.test[pred.probs.test > 0.5] <- "Yes"
      
      # compute validation set error
      # confusion matrices to check the accuracy calculated is correct
      #confusionMatrix(data=pred.label.test, reference=validationData$default, positive="Yes")
      #table(PredictionTest=pred.label.test, TruthTest=validationData$default)
      
      #mean("Yes" == validationData$default) # random chance
      #accuracy = mean(pred.label.test == validationData$default); accuracy
      validationTestErrorRate = mean(pred.label.test != validationData$default)
      return(validationTestErrorRate)
      
}

logisticValidationRunner2()
# 0.0264
# this is slightly lower than previous test error estimates, but not by much. 
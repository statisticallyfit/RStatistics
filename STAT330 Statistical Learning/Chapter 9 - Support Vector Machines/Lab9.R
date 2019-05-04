setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 9 - Support Vector Machines")

library(e1071)
# another option is LiblineaR, good for large linear problems

# cost = small = tuning param C = large (wide margins)
# cost = large => tuning param C = small (narrow margins)

# SUPPORT VECTOR CLASSIFIER ---------------------------------------------------------------

set.seed(1)
x = matrix(rnorm(20 * 2), ncol=2)
y <- c(rep(-1, 10), rep(1,10))
x[y == 1, ] <- x[y == 1, ] + 1 # increment first half of x by one


# plot to check if classes are linearly separable
plot(x, col=(3-y))
# NOT separable by linear boundary


# fit the support vector classifier (must encode y = factor to avoid SVM regression)
data <- data.frame(x = x, y=as.factor(y))
# scale = false means not to scale each predictor to have mean 0 and sd = 1
svm.fit <- svm(y ~ ., data=data, kernel="linear", cost=10, scale=FALSE)
str(svm.fit)

# Plot the resulting SVC
plot(svm.fit, data)


# NOTE: support vectors are plotted as crosses, (indicating up to where there is the margin)
# and other observations are plotted as circles
# Get the support vector indices (the id to which observation they correspond)
# so obs 1, 2, 5, 7 ... are within or on the margin
svm.fit$index


# INFO
summary(svm.fit)
# linear kernel
# cost = 10
# seven support vectors, with 4 in one class (-1) and three in the other class (1)


# Smaller value of cost (means higher C tuning means wider margins means more violations allowed)
svm.fit2 <- svm(y ~ ., data=data, kernel="linear", cost=0.1, scale=FALSE)
plot(svm.fit2, data)
svm.fit2$index
# much more support vectors since margins are wider

# INFO that is NOT GIVEN: the coeffs of Linear Decision Boundary obtained with the SVC is fit
# AND the width of the margin


# CROSS-VALIDATION for SUPPORT VECTOR CLASSIFIER ----------------------

# tine() = ten - fold cross validation on set of models
set.seed(1)

# ranges = range of values for cost parameter
tune.out <- tune(svm,  y ~ ., data=data, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

tune.out$best.parameters
tune.out$performances
tune.out$best.model$coefs

# result: cost = 0.1 gave the lowest cross-validation error rate. 
# Best model info
summary(tune.out$best.model)


# Test Performance: for cross-validation chosen Cost value:
X.test <- matrix(rnorm(20*2), ncol=2)
Y.test <- sample(c(-1,1), size=20, replace=TRUE)
X.test[Y.test ==1, ] = X.test[Y.test == 1, ] + 1 # follow same steps as in training set
testData <- data.frame(x=X.test, y = as.factor(Y.test))

pred <- predict(tune.out$best.model, testData) # use best model chosen by cross-validation

# confusion matrix
table(Prediction=pred, Truth=testData$y)

# so with this value of cost, 19 of test observations are correctly classified
testError <- mean(pred != testData$y) # one observation is missclassified
testError
# 1/20



# Try again with lower cost (this time computing testError with self-chosen cost, not with CV chosen cost)
# note: "data" here  = traindata
svm.fit3 <- svm(y ~ ., data=data, kernel="linear", cost=0.01, scale=FALSE)
pred <- predict(svm.fit3, testData)
table(Pred=pred, Truth=testData$y) # so 2 observations are missclassified
# test error
mean(pred != testData$y)



# Situation: classes are actually linearly separable
# simulate the data to be linearly separable:
x[y == 1, ] = x [y ==1, ] + 0.5
plot(x, col=(y+5)/2, pch=19) # see? separable linearly

# Fit model using very large cost so very narrow margins (no missclassifications allowed)
data <- data.frame(x = x, y = as.factor(y))
svm.sep.fit <- svm(y ~ ., data=data, kernel="linear", cost=1e5)
summary(svm.sep.fit)

pred <- predict(svm.sep.fit, testData)
table(Pred = pred, Truth=testData$y)
# HELP: how to tell no training errors were made?

svm.sep.fit$index # got 3 support vectors, one in class (-1), the other in class (1)
# few support vectors because margins are narrower. 


# Try smaller cost value, since it seems likely the model will perform poorly on test data
# because the margins are narrow because that implies high variance-model (overfit)
svm.sep.fit2 <- svm(y ~., data=data, kernel="linear", cost=1)
summary(svm.sep.fit2)
plot(svm.sep.fit2, data)

pred <- predict(svm.sep.fit2, testData)
table(Pred = pred, Truth=testData$y) # 1 missclassified training observation (-1, 1)
# but also margin is wider and there are 7 support vectors

svm.sep.fit2$index

# seems likely this model (less flexible) will do better on test data than the narrow-margin
# (flexible) model with cost = 1e5



### SUPPORT VECTOR MACHINE -----------------------------------------------------------------

# using nonlinear kernel

# gamma = specifies value for radial kernel
# d = specifies degree for polynomial kernel

set.seed(1)
x = matrix(rnorm(200*2), ncol=2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 2
y = c(rep(1,150), rep(2, 50))
data = data.frame(x = x, y = as.factor(y))

# see that class boundary is indeed nonlinear
plot(x, col=y)


# split data into test and train
trainIndices <- sample(200, size=100)
testData <- data[-trainIndices, ]
trainData <- data[trainIndices, ]

svm.radial <- svm(y ~ ., data=trainData, kernel="radial", gamma=1, cost=1)

# Shows: there are many training errors (the opposite colors?)
plot(svm.radial, trainData)

# INFO
summary(svm.radial)


# Increasing cost -> reducing C tuning param -> narrow margins -> reduce train errors but also
# risk of overfitting
svm.radial2 <- svm(y ~., data=trainData, kernel="radial", gamma=1, cost=1e5)
plot(svm.radial2, trainData)  # more flexible: more irregular boundary, fitted hard to data


# SVM USING CROSS-VALIDATION to select best COST AND GAMMA:  --------------
set.seed(1)

tune.out <- tune(svm, y ~., data=trainData, kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000),gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)
# best cost = 1, best gamma = 2

# Find test set predictions
pred <- predict(tune.out$best.model, newx=testData)
# confusion matrix
table(Pred=pred, Truth = testData$y)

# test error
mean(pred != testData$y)
# so 39% of test observations are missclassified. 




### ROC CURVES ----------------------------------------------------------------------------
library(ROCR)

# writing function to plot roc curve given a vector containinga  numerical score for each 
# observation, pred, and a vector containing the class label for each observation, truth

plotROCCurve <- function(pred, truth, ...) {
      pred.Obj <- prediction(pred, truth)
      perf <- performance(pred.Obj, "tpr", "fpr")
      plot(perf, ...)
}

# Can use fitted values for each observation, which are the numerical scores used to obtain
# the class labels. 
# If fitted value (f(x)) > 0 then observation is assigned to one class, else to the other class.

# To obtain fitted values while doing svm, use decision.values=TRUE
# Then predict() will output the fitted values, not just the class labels. 

# Fit the model, using Cross-validation-chosen gamma and cost
svm.radial.fitted <- svm(y ~., data=trainData, kernel="radial", gamma=2, cost=1,
                         decision.values=TRUE)
# Increase gamma to get more flexible fit
svm.radial.fitted2 <- svm(y ~., data=trainData, kernel="radial", gamma=50, cost=1, 
                          decision.values=TRUE)

par(mfrow=c(1,2))

# Evaluate prediction accuracy for train data
# note: these predictions are on training data
fitted.train.gamma2 <- attributes(predict(svm.radial.fitted, trainData, decision.values = TRUE))$decision.values
fitted.train.gamma50 <- attributes(predict(svm.radial.fitted2, trainData, decision.values = TRUE))$decision.values
plotROCCurve(fitted.train.gamma2, trainData$y, main="Training Data Gamma = 2") # SVM seems to produce accurate predictions
plotROCCurve(fitted.train.gamma50,trainData$y, add=TRUE, col="red", main="Training Data, Gamma = 50")
# does well on training data

# Evaluate Prediction Accuracy for TEST DATA
fitted.test.gamma2 <- attributes(predict(svm.radial.fitted, testData, decision.values=TRUE))$decision.values
fitted.test.gamma50 <- attributes(predict(svm.radial.fitted2, testData, decision.values=TRUE))$decision.values
plotROCCurve(fitted.test.gamma2, testData$y, main="Test Data for Gamma = 2")
plotROCCurve(fitted.test.gamma50, testData$y, main="Test Data for Gamma = 50", add=TRUE, col="red")


# So the more flexible (gamma = 50) model does better on train and worse on test (higher variance)



### SVM WITH MULTIPLE CLASSES ---------------------------------------------------------------

# (does one versus one approach)

set.seed(1)
x = rbind(x, matrix(rnorm(50*2), ncol=2))
y = c(y, rep(0, 50))
x[y == 0, 2] = x[y == 0, 2] + 2
data = data.frame(x = x, y = as.factor(y))
par(mfrow=c(1,1))
plot(x, col=(y+1)) # 3 classes with nonlinear boundary

# Fit the SVM to the data
svm.multi <- svm(y ~ ., data=data, kernel="radial", cost=10, gamma=1)
plot(svm.multi, data)



### APPLICATION TO GENE EXPRESSION DATA -----------------------------------------------------
library(ISLR)
data("Khan")

# Data = consists of a number of tissue samples corresponding to four distinct types of 
# small round blue cell tumors. Gene measurements are available for each tissue sample. 
head(Khan)
class(Khan)
str(Khan)
names(Khan)
dim(Khan$xtrain) # 2308 genes (predictors in columns), and 63 observations (rows)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

# train set = 63 observations
# test set = 20 obsevations
table(Khan$ytrain)
table(Khan$ytest) # counts of the types of numbers (1,2,3,4) that there are


# GOAL: predict cancer subtype using gene expression measurements. 

# note: there are many features (p , gene expressions, 2308) relative to n = num obs = 63
# SUGGESTS: we should use linear kernel since the additional flexibility resulting
# from polynomial or radial kernel is unnecessary (already too high variance?)

khanTrain <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
khanTest <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))

khan.svm <- svm(y ~ ., data=khanTrain, kernel="linear", cost=10)
summary(khan.svm)
# wide margins, many support vectors, more restrictive (less flexible, low variance) model

# Train Performance: THERE ARE NO TRAINING ERRORS (because no off-diag elements are non-zero)
table(khan.svm$fitted, khanTrain$y)
# not surprising because the large number of features relative to obs implies it is easy to 
# find hyperplanes that fully separate classes.

# Test Error Calculations: 
pred.khan <- predict(khan.svm, newdata=khanTest)
# confusion matrix
table(Pred = pred.khan, Truth=khanTest$y) 
# just 2 test missclassifications! (predicted 2 when it was 3)

# test error
mean(pred.khan != khanTest$y)

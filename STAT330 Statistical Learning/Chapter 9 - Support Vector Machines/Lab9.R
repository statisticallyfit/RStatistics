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


# Test set predictions, at any cost parameter: 

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

svm.sep.fit$index # got 3 support vectors, one in class (-1), the other in class (1)

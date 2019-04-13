setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 6 - Lasso, Subset, Ridge, PCR/")
library(ISLR)
library(ggplot2)
library(gridExtra)
library(MASS)

library(leaps)
library(glmnet)
library(pls)

data("College")
head(College)

# a)
set.seed(11)
any(is.na(College))

N <- nrow(College); N

trainIndices <- sample(1:N, size=N / 2)
testIndices <- -trainIndices
collegeTrain <- College[trainIndices, ]
collegeTest <- College[testIndices, ]
X <- model.matrix(Apps ~ . -1, data=College)
Y <- College$Apps
X.train <- model.matrix(Apps ~ . -1, data=collegeTrain)
X.test <- model.matrix(Apps ~ . -1, data=collegeTest)
Y.train <- collegeTrain$Apps
Y.test <- collegeTest$Apps 

# b) linear model OLS on training set and report test error
college.lm <- lm(Apps ~., data=collegeTrain)
pred.lm <- predict(college.lm, collegeTest)
testError.lm <- mean((Y.test - pred.lm)^2)
testError.lm
# 1538442 

# c) RIDGE regression on training set with lambda fit by cross validation
lambdaGrid <- 10^seq(4, -2, length=100)
college.ridge <- cv.glmnet(X.train, Y.train, alpha=0, lambda=lambdaGrid, thresh=1e-12)
lambdaBest.ridge <- college.ridge$lambda.min
lambdaBest.ridge
# 18.73817

# Now calculate the test MSE with this minimum lambda
pred.ridge <- predict(college.ridge, newx=X.test, s=lambdaBest.ridge)
testError.ridge <- mean( (Y.test - pred.ridge)^2 )
testError.ridge
# 1608660
# higher than OLS


# d) LASSO model on training set with lambda fit by CV on training set
college.lasso <- cv.glmnet(X.train, Y.train, alpha=1, lambda=lambdaGrid, thresh=1e-12)
lambdaBest.lasso <- college.lasso$lambda.min
lambdaBest.lasso
# 21.54435

# Now calculate test MSE with this minimum lambda, for the lasso model
pred.lasso <- predict(college.lasso, newx=X.test, s = lambdaBest.lasso)
testError.lasso <- mean( (Y.test - pred.lasso)^2 )
testError.lasso
# 1635280
# also higher than OLS

# Coefficients of final model with this lambda
college.final.lasso <- glmnet(X, Y, alpha=1)
predict(college.final.lasso, s = lambdaBest.lasso, type="coef")



# e) PCR
college.pcr <- pcr(Apps ~ ., data=collegeTrain, scale=TRUE, validation="CV")
validationplot(college.pcr, val.type="MSEP")

# how many componnets give min CV error?
msepObj <- MSEP(college.pcr)
names(which.min(msepObj$val[1,1,]))
# 17 components, but let's use the similar value of 10 
msepObj$val[1,1,10]
msepObj$val[1,1,17]
# similar CV errors for componnets 10 and 17

# get test error with the 10 components
pred.pcr <- predict(college.pcr, collegeTest, ncomp=10)
testError.pcr <- mean( (Y.test - pred.pcr)^2 )
testError.pcr
# 3014496


# f) PLS model
college.pls <- plsr(Apps ~ ., data=collegeTrain, scale=TRUE, validation="CV")
validationplot(college.pls, val.type="MSEP")

msepObj <- MSEP(college.pls)
names(which.min(msepObj$val[1,1,]))
# 12 components, but let's use the similar value of 10 
msepObj$val[1,1,12]
msepObj$val[1,1,10]

pred.pls <- predict(college.pls, collegeTest, ncomp=10)
testError.pls <- mean( (Y.test - pred.pls)^2 )
testError.pls
# 1508987 # the only one lower than OLS test error

setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 6 - Lasso, Subset, Ridge, PCR/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(ggplot2)
library(gridExtra)
#library(leaps)

library(pls)
# installed with conda


data("Hitters")
hitData <- na.omit(Hitters)

set.seed(2)

# scale = standardizes each predictor
# validation = cv means pcr computes the tenfold cross validation error for each M (number of
# principal components used)
hit.pcr <- pcr(Salary ~., data=hitData, scale=TRUE, validation="CV")
hit.pcr$coefficients
names(hit.pcr)

summary(hit.pcr) # note tehse CV's are the Root mean squared errors. Must square these to get MSE

# plot R2
validationplot(hit.pcr, val.type="R2")
# plot uroot mean squared error CV estimates
validationplot(hit.pcr)
# plot MSE (usual) square of RMSE
validationplot(hit.pcr, val.type="MSEP") # to plot the MSE's

# smallest cross validation error is when M = 16
# Barely fewer than M = 19 (OLS case)
# But cross-validation error is roughly the same when only one component is included in the model
# Suggests just use one component or small num of comonents

# Also:
# Percentage of variance explained in the predictors and response and predictors, 
#using different number of components. 
# This is the amount of info about the predictors or response that is captured using M
# principal components. 
# So using M = 1 captures only 38 pct of all variance in predictors. 
# Using M = p = 19 turns this to 100%


# To get test set performance: do PCR on training data and evaluate the test set error
set.seed(1)
X = model.matrix(Salary ~., hitData)[,-1] # get rid of intercept (?) why?
X
Y = hitData$Salary
trainIndices = sample(1:nrow(X), nrow(X)/2)
testIndices <- -trainIndices
hit.train.pcr <- pcr(Salary ~., data=hitData[trainIndices, ], scale=TRUE, validation="CV")

validationplot(hit.train.pcr, type="MSEP")
# From plot it looks like lowest cross validation test error estimate occurs at M = 7

# Computing test error on the training model for M = 17
M = 7 
predPCR <- predict(hit.train.pcr, X[testIndices, ], ncomp = M) # predicting training with M = 17
testErrorPCR <- mean( (predPCR - Y[testIndices])^2 )
testErrorPCR
# 96556.22

# Seems better than lasso and ridge test MSE


# Final step: fit PCR on full data set using M = 7 to get the final model
hit.bestfull.pcr <- pcr(Y ~ X, scale=TRUE, ncomp=M)
summary(hit.bestfull.pcr)



## PARTIAL LEAST SQUARES -----------------------------------------------------------------
set.seed(1)
hit.train.pls <- plsr(Salary ~., data=hitData[trainIndices, ], scale=TRUE, validation="CV")
summary(hit.train.pls)

validationplot(hit.train.pls, type="MSEP")
# seems lowest test error at M = 2

# compute test error for this M = 2 model
M = 2
predPlsM2 <- predict(hit.train.pls, X[testIndices, ], ncomp=M)
testErrorPls <- mean( (predPlsM2 - Y[testIndices])^2 ); testErrorPls
# 101417.5

# slightly higher than test mse obtained using ridge, lasso, and pcr


# Do PLS with full data set (using this best M = 2 value) to get the final model
hit.bestfull.pls <- plsr(Salary ~., data=hitData, scale=TRUE, ncomp=M)
summary(hit.bestfull.pls)
# see percentage of variance in salary that the two-component PLS fit explains is 46 p, almost
# as much as that explained using the seven component PCR, which is 46.69 p
# This is because PCR only attempts to maximize the amount of variance explained in the 
# predictors while PLS searches for directions that explain variance in both predictors
# and response. 
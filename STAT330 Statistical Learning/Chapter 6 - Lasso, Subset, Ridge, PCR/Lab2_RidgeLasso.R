setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 6 - Lasso, Subset, Ridge, PCR/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(ggplot2)
library(gridExtra)
library(leaps)

library(glmnet)
# install.packages("glmnet") # used conda command



data("Hitters")
hitData <- na.omit(Hitters)

X = model.matrix(Salary ~., hitData)[,-1] # get rid of intercept (?) why?
X
Y = hitData$Salary
 # note: model matrix automatically transforms categorical vars into dummy vars , good since
# glmnet can only take numerical (quantitative input)


### RIDGE REGRESSION ------------------------------------------------------------------------
# alpha = 0 if ridge, alpha = 1 if lasso
# note: glmnet standardizes the variables

lambdaGrid = 10^seq(10, -2, length=100) # lambdas, range from 10^10 to 10^-2
# (covers full scenarios from the null model of intercept to OLS fit)
hit.ridge <- glmnet(X, Y, alpha=0, lambda=lambdaGrid) # lambda vlues in a grid

# associate with each lambda is a vector of ridge coeffs, stored in matrix accessed by coef()
# In this case it is a 20 x 100 matrix, 20 rows for each predictor and intercept and 100 cols
# for each value of lambda. 
dim(coef(hit.ridge)) 

lambdaGrid[50]

# when a large lambda is used, the estimated ridge coeffs are smaller than OLS coefs
# Getting ridge coeffs for 50th lambda
coef(hit.ridge)[,50] # rows = predictors, cols = lambda (get the 50th column, all rows)

sqrt(sum(coef(hit.ridge)[-1, 50]^2)) # the l-2 norm of the coefficients, excluding intercept
# at 50th lambda

# Here are coefficients for lambda = 705 (much smaller than the 50th lambda)
coef(hit.ridge)[,60]
# value of 60th lambda
hit.ridge$lambda[60]
# L-2 norm of the coefs at 60th lambda
sqrt(sum(coef(hit.ridge)[-1, 60]^2))

# Predict to get the ridge coeffs for new value of lambda, say lambda = 50
predict(hit.ridge, s=50, type="coefficients")[1:20, ]



# Split samples into train and test to estimate test error of ridge and lasso regressions

set.seed(1)
trainIndices = sample(1:nrow(X), nrow(X)/2)
testIndices <- -trainIndices
Y.Test <- Y[testIndices]
Y.Test

# Fit ridge model on training test and evaluate its MSE on test set, using lambda = 4
# Get preidctions for a test set using newx argumnet
hit.train.ridge <- glmnet(X[trainIndices, ], Y[trainIndices], alpha=0, lambda=lambdaGrid, 
                          thresh=1e-12)
preLambda4 = predict(hit.train.ridge, s=4, newx= X[testIndices, ])
mean( (preLambda4 - Y[testIndices])^2 )
# 101036.8 # is the test MSE

# If we had fit a model with just an intercept we could have predicted each test observation
# using the mean of the training observations, then can compute test MSE this way
mean( (mean(Y[trainIndices]) - Y[testIndices])^2 )
# 193253.1

# can get same value by fitting ridge model with very large lambda
predlarge = predict(hit.train.ridge, s=1e10, newx=X[testIndices, ])
mean( (predlarge - Y[testIndices])^2 )
# 193253.1

# Comment: so fitting ridge model with lambda = 4 leads to much lower test error than  fitting
# model with just an intercept (101036 as opposed to 193253.1)
# First extreme: lambda = large => shrinkage coefs near 0

# Check if any benenfit to using lambda = 4 rather than just OLS (lambda = 0) 
# Second extreme: lambda = 0 => shrinkage coefs = OLS coefs
predLambda0 <- predict(hit.train.ridge, s=0, newx = X[testIndices, ], extract=TRUE)
predLambda0
mean( (predLambda0 - Y[testIndices])^2 )
# 114723.6 is test error
# so test error is still lower for lambda = 4


# Can use the cv.glmnet to choose the lambda value. Performs ten-fold cross validation
set.seed(1)
cv.out <- cv.glmnet(x=X[trainIndices, ], y=Y[trainIndices], alpha=0)
cv.out$lambda
cv.out$cvm
bestLambda = cv.out$lambda.min # this is the log(trueminlambda) = 211
bestLambda

plot(cv.out) # plots log lambda against MSE

# get test mse associated with this value of lambda
predBestLambda = predict(hit.train.ridge, s=bestLambda, newx = X[testIndices, ])
testErrorBestLambda = mean( (predBestLambda - Y[testIndices])^2 )
testErrorBestLambda
# 96015.51

# Fit the model using this lambda
hit.fullBestLambda.ridge <- glmnet(X, Y, alpha=0)
predict(hit.fullBestLambda.ridge, type="coefficients", s=bestLambda)[1:20,]

# as expected, no coef is zero since ridge does not do variable selection



### LASSO ---------------------------------------------------------------------------------

hit.train.lasso <- glmnet(X[trainIndices, ], Y[trainIndices], alpha=1, lambda=lambdaGrid)
hit.train.lasso
plot(hit.train.lasso) # Plots L1 norm against coefficients (some turn exactly 0)

# Do cross validation and compute associated test error
set.seed(1)
cv.out <- cv.glmnet(X[trainIndices, ], Y[trainIndices], alpha=1)
bestLambda.lasso <- cv.out$lambda.min; bestLambda.lasso
predBestLambda.lasso = predict(hit.train.lasso, s=bestLambda.lasso, newx=X[testIndices, ])

testErrorBestLambda.lasso = mean( (predBestLambda.lasso - Y[testIndices])^2 )
testErrorBestLambda.lasso
# 100743.4 # much lower than test mse of OLS and null and similar to MSE of ridge with cv-chosen lambda

# still, feature selection is a pro
hit.bestlambda.lasso <- glmnet(X, Y, alpha=1, lambda=lambdaGrid)
coefsBestLambda <- predict(hit.bestlambda.lasso, type="coefficients", s=bestLambda.lasso)[1:20,]
coefsBestLambda[coefsBestLambda != 0]

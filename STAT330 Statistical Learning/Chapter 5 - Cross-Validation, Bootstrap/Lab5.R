setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 5 - Cross-Validation, Bootstrap/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(MASS)
#library(car)
#library(caret)

options(show.signif.stars = FALSE)

set.seed(1)

trainIndices = sample(1:392, size=196)

auto.cv.lm <- lm(mpg ~ horsepower, data=Auto, subset=trainIndices)

### Estimate response for all 392 observations
fits <- predict(auto.cv.lm, Auto)
overallMSE <- mean( (Auto$mpg - fits)[-trainIndices] ^2); overallMSE
MSE(auto.cv.lm, Auto) # HELP why is this different?


#testData <- data.frame(mpg=Auto$mpg[-trainIndices], 
#                       horsepower=Auto$horsepower[-trainIndices])
#testY <- Auto$mpg[-trainIndices]
#preds = predict(auto.cv.lm, testData)
#overallMSE <- mean( (testY - preds)^2 ); overallMSE

# So the estimated test MSE for the linear regression fit is 26.14


# Calculate MSE for the polynomial models
poly2.lm = lm(mpg ~ poly(horsepower, 2), data=Auto, subset=trainIndices)
mean( (Auto$mpg - predict(poly2.lm, Auto))[-trainIndices]^2 )
# MSE(poly2.lm, Auto) # Help why isn't this working??

poly3.lm = lm(mpg ~ poly(horsepower, 3), data=Auto, subset=trainIndices)
mean( (Auto$mpg - predict(poly3.lm, Auto))[-trainIndices]^2 )



# Illustrating variability in validation set test errors by choosing different
# training partitions
set.seed(2)
trainIndices = sample(1:392, 196)
auto.cv.lm2 <- lm(mpg ~ horsepower, subset=trainIndices, data=Auto)
mean( (Auto$mpg - predict(auto.cv.lm2, Auto))[-trainIndices]^2 )
# 23.30 

poly2.lm2 = lm(mpg ~ poly(horsepower, 2), data=Auto, subset=trainIndices)
mean( (Auto$mpg - predict(poly2.lm2, Auto))[-trainIndices]^2 )
# 18.90124

poly3.lm2 = lm(mpg ~ poly(horsepower, 3), data=Auto, subset=trainIndices)
mean( (Auto$mpg - predict(poly3.lm2, Auto))[-trainIndices]^2 )
# 19.2574

# Overall: quadratic is better than linear and cubic. 

### LEAVE-ONE-OUT CROSS VALIDATION (LOOCV) ---------------------------------------------

# note: using glm to fit the linear model because we can use the cv.glm()
library(boot)
auto.cv.glm <- glm(mpg ~ horsepower, data=Auto)
loocv.testError <- cv.glm(data=Auto, glmfit=auto.cv.glm)
loocv.testError$delta
# 24.315 is the cross-validation estimate for the test error

# Getting several CV(n) results
replications <- 5
CV.n.errors <- rep(0, replications) # n = sample size n
for(i in 1:replications){
      glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
      CV.n.errors[i] = cv.glm(data=Auto, glm.fit)$delta[1]
}
CV.n.errors
# [1] 24.23151 19.24821 19.33498 19.42443 19.03321]

# Comment: sharp drop in estimated testMSE as the polynomial order goes up. 

### K-FOLD CROSS VALIDATION ----------------------------------------------------------

# note: using K = 10 because K = 10 folds is a common choice for number of groups
# to make in the data. 

set.seed(17)
numReps = 10
CV.k.errors = rep(0, numReps)

for(i in 1:numReps){
      glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
      CV.k.errors[i] = cv.glm(Auto,  glm.fit, K = 10)$delta[1]
}
CV.k.errors
#  [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609 19.71201 18.95140 19.50196

# Comment: still lower estimated test error rates for the higher order polynomials

# note: the two deltas for the k-fold CV are: first is the usual k-fold CV test error
# and thes econd delta is a bias-corrected version. 

### BOOTSTRAP --------------------------------------------------------------------------
library(boot)

# alpha is the statistic we want to estimate
data("Portfolio")

# function that calculates the statistic of interest, theta = H(x)
alpha.fn <- function(data, index) {
      X = data$X[index]
      Y = data$Y[index]
      return ((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

head(Portfolio)

# Randomly select observations in the range 1:100 with replacement (boostrap indices)
set.seed(1)
# This output is one boostrap estimate. Repeated many times, we get that many
# boostrap estimates of the statistic. Then we can calculate their standard deviation. 
alpha.fn(Portfolio, index = sample(1:100, size=100, replace=TRUE))

# However boot() automates this approach
b = boot(data=Portfolio, statistic=alpha.fn, R=1000) # 1000 replications
# the alpha estimate is 0.5758 with SE(alpha-hat) = 0.0886

b$t0 # alpha-hat (estimate of alpha using repeated samples)
b$t # is the vector containing individual alpha-hat estimates, for each boot sample
mean(b$t) # is equal to b$t0
b$statistic(Portfolio, 1:100) # statistic() is the stored function


## Using BOOTSTRAP to estimate Accuracy of Linear REgression Model (assess variability
# of the coefficients and predictions)

# function to take fitting data (entire Auto set) and set of indices for the
# observations to use. Returns coefficient estimates
boot.fn <- function(data, index) { #index = training index set
      return(coef(lm(mpg ~ horsepower, data=data, subset=index)))
}
boot.fn(Auto, 1:392) # coef estimates for entire data set


# Bootstrap manual
set.seed(1)
boot.fn(Auto, sample(1:392, size=392, replace=TRUE))
#(Intercept)  horsepower 
#38.7387134  -0.1481952 
boot.fn(Auto, sample(1:392, size=392, replace=TRUE))
# (Intercept)  horsepower 
# 40.0383086  -0.1596104 

# Each time, get different boot estimates for each resampling

# Boostrap automatic
boot(data=Auto, boot.fn, R=1000)
# intercept boostrap estimate = 39.935 with std error 0.86
# slope horspower estimate = -0.157 with stderror 0.007

# these are close to the real OLS estimates: 
summary(lm(mpg ~ horsepower, data=Auto, subset=1:392))$coef[,1:2]

# The estimates are slightly different. 
# The linear model does not take into account non-linearlity in the data, so 
# residuals are inflated and s^2 is inflated
# Also assumes the x_i are fixed and all variability comes from variation in the
# errors e_i (?)

# Boostrap does not rely on these assumptions so is giving more accurate estimate
# of the B0 and B1 than OLS


# Example 2: boostrap for quadratic model
boot.quad.fn <- function(data, index){
      coef(lm(mpg ~ horsepower + I(horsepower^2), data=data, subset=index))
}
set.seed(1)

# Doing the bootstrap
boot(Auto, boot.quad.fn, R=1000)
# B0-hat = 56.9 with std.error 2.09
# B1-hat = -0.466 with std.error = 0.03341
# B2-hat = 0.0012 with std.error 0.0001208

# Compare to OLS estimates
summary(lm(mpg ~ horsepower + I(horsepower^2), data=Auto))$coef

# The closeness of the coeff estimates and std error estimates indicates that the
# quadratic model better explains variation in Y, since boostrap relies on fewer
# assumptions so is most likely to be correct. 
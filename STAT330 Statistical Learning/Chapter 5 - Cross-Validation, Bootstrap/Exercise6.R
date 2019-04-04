setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 5 - Cross-Validation, Bootstrap/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(boot)
#library(caret)

data("Default")

# Using bootstrap to predict estimates of standard errors of the income and balance
# in logistic coefficients

# a)
default.glm <- glm(default ~ income + balance, data=Default, family=binomial)
summary(default.glm)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)
# (Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16
# income       2.081e-05  4.985e-06   4.174 2.99e-05
# balance      5.647e-03  2.274e-04  24.836  < 2e-16


# b) 
# data = the Default data
# index = index array of observations (like train indices)
# OUTPUT: set of coefficient estimates for the predictors in above model
boot.fn <- function(data=Default, index) {
      return( coef(default.glm <- glm(default ~ income + balance, 
                               data=data, family=binomial, subset=index)))
}

# Doing the bootstrap
set.seed(1)
boot(data=Default, statistic = boot.fn, R = 50)
# Bootstrap Statistics :
        # original        bias     std. error
# t1* -1.154047e+01  1.285967e-01 4.419170e-01
# t2*  2.080898e-05 -5.379014e-08 4.398295e-06
# t3*  5.647103e-03 -7.608258e-05 2.471941e-04

summary(default.glm)$coef[,1:2]

# Comment: coefficient estimates are identical with slightly different std errors. 
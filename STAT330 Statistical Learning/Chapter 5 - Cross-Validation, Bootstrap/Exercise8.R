setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 5 - Cross-Validation, Bootstrap/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ggplot2)
library(boot)

# Goal: cross-validation on simulated data set

# a) generate data
n = 100

set.seed(1)
y = rnorm(n)
x = rnorm(n)
y = x - 2*x^2 + rnorm(n)

p = 2 # X1 = x, X2 = -2*x^2, rnorm(100 is the error)

# Y = X - @X^2 + errors 
# where errors ~ Normal(0, 1)



# b) scatterplot
quadNormalData <- data.frame(x, y)
ggplot(data=quadNormalData, aes(x=x, y=y)) + geom_point(shape=19)
# negative quadratic!


# c) compute the LOOCV errors that result from fitting the four models (poly order 4)


loocvPolyRunner <- function() {
      linear.glm <- glm(y ~ x, data=quadNormalData)
      loocv.1 <- cv.glm(data=quadNormalData, glmfit=linear.glm)# test error for linear
      
      poly2.glm <- glm(y ~ poly(x, 2), data=quadNormalData)
      loocv.2 <- cv.glm(data=quadNormalData, glmfit=poly2.glm)# test error for linear
      
      poly3.glm <- glm(y ~ poly(x, 3), data=quadNormalData)
      loocv.3 <- cv.glm(data=quadNormalData, glmfit=poly3.glm)# test error for linear
      
      poly4.glm <- glm(y ~ poly(x, 4), data=quadNormalData)
      loocv.4 <- cv.glm(data=quadNormalData, glmfit=poly4.glm)# test error for linear
      
      return(rbind(loocv.1$delta, loocv.2$delta,loocv.3$delta, loocv.4$delta))
}
set.seed(1)
loocvPolyRunner()

#   LOOCVTestErrors
# 1        5.890979
# 2        1.086596
# 3        1.102585
# 4        1.114772



# d) repeat using another random seed
set.seed(10)
loocvPolyRunner()

# All the exact same since LOOCV evaluates n folds of a single observation. 


# e) 
# quadratic model had smallest LOOCV test error estimate, makes sense because data
# is quadratic, so best fit is provided by quadratic model. 

# f) compare with OLS
summary(lm(y ~ x, data=quadNormalData))
summary(lm(y ~ poly(x, 2), data=quadNormalData))
summary(lm(y ~ poly(x, 3), data=quadNormalData))
summary(lm(y ~ poly(x, 4), data=quadNormalData))

# quadratic is again the best model, but its R^2 is lower than that for model 4

# NOTE: just need the 4th order model to see the quadratic term is significant but
# the others are not. 
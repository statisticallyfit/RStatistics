setwd("/development/projects/statisticallyfit/github/R/RStatistics/STAT270 Inference/ASSIGNMENTS")

options(show.signif.stars = FALSE)


# part a) entering data and saving as tab-delimited file, 
# using read.table to open data. ----------------------------------------------------
crateData <- read.table("chargeData.csv", header=TRUE)
crateData
# OUTPUT
#    x   y
# 1  14  68
# 2  23 105
# 3   9  40
# 4  17  79
# 5  10  81
# 6  22  95
# 7   5  31
# 8  12  72
# 9   6  45
# 10 16  93


# part b) estimating the regression line --------------------------------------------

# METHOD 1: the lm-object method (automatic)
crate.lm <- lm(y ~ x, data=crateData)
summary(crate.lm)

# OUTPUT: regression line is: 
# Y-hat = 22.4048 + 3.618 x


# METHOD 2: matrix method
n <- nrow(crateData)
Y = matrix(crateData$y, ncol=1)
X = matrix(c(rep(1, n), crateData$x), ncol=2)

beta.hat = solve(t(X) %*% X) %*% (t(X) %*% Y); beta.hat

# OUTPUT:
#           [,1]
# [1,] 22.404762
# [2,]  3.619048

# Therefore the two methods are the same, we get the same regression line of
# Y-hat = 22.4048 + 3.618 x
#, where B0-hat = 22.4048, B1-hat = 3.618


# part c) finding 95% confidence interval for slope and intercept: -----------------

# step 1: calculate SSE:
x = crateData$x
SSE = t(Y) %*% Y - t(beta.hat) %*% (t(X) %*% Y); SSE
#          [,1]
# [1,] 1116.119

# step 2: s^2 = SSE / (n-(k+1)), where k = number of beta parameters, not including intercept
k = 1
s = sqrt(SSE / (n-k-1)); s
#          [,1]
# [1,] 11.81164


# Calculate critical t-value. There are df = n - (k+1) = 10 - 2 = 8
t.crit = abs(qt((1-0.95)/2, df=n-(k+1))); t.crit
# 2.306004

# Create the vector a = (1,0) to represent we are calculating the intercept CI. 
a = matrix(c(1,0), ncol=1)

# 95% Confidence interval for Intercept, B0: 
intercept.hat = t(a) %*% beta.hat; intercept.hat
#          [,1]
# [1,] 22.40476
CI.B0 = intercept.hat + c(-1,1) * t.crit * s * sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
CI.B0
# [1] 0.9340886 43.8754352


# 95% Confidence interval for Slope: B1: 
# create the vector a = (0,1) for slope calculation
a = matrix(c(0,1), ncol=1)
slope.hat = t(a) %*% beta.hat; slope.hat
#          [,1]
# [1,] 3.619048
CI.B1 = slope.hat + c(-1,1) * t.crit * s * sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
CI.B1
# [1] 2.151343 5.086753



# part e) plot of predicted line with scatterplot of points ------------------------
library(ggplot2)

fit.data <- data.frame(fit = crate.lm$fitted.values, x=x, y=Y)

ggplot(crateData, aes(x=x, y=y)) + 
      geom_point(shape=19, size=3) +
      geom_line(data=fit.data, aes(y=fit), colour="dodgerblue",size=1) +
      ggtitle("Predicted Values for Distance to Freight Charge") + 
      xlab("Distance (hundreds of miles)") + ylab("Freight Charge ($)")




# part f) predict mean charge with distance 2000 miles ( 20 hundred miles) ----------

# Create the vector a for prediction
x.star = 20
a = matrix(c(1, x.star), ncol=1)

predicted.value = t(a) %*% beta.hat; predicted.value
#          [,1]
# [1,] 94.78571


t.crit = abs(qt((1-0.90)/2, df=n-k-1)); t.crit
# [1] -1.859548

CI.mean = predicted.value + c(-1,1) * t.crit * s * sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
CI.mean

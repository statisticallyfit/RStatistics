setwd("/development/projects/statisticallyfit/github/R/RStatistics/STAT270 Inference/ASSIGNMENTS")

options(show.signif.stars = FALSE)


# part a) entering data and saving as tab-delimited file, using read.table to open data.
invoiceData <- read.table("chargeData.csv", header=TRUE)
invoiceData
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


# part b) estimating the regression line

# METHOD 1: the lm-object method (automatic)
crate.lm <- lm(y ~ x, data=invoiceData)
summary(crate.lm)

# OUTPUT: regression line is: 
# Y-hat = 22.4048 + 3.618 x


# METHOD 2: matrix method
n <- nrow(invoiceData)
Y = matrix(invoiceData$y, ncol=1)
X = matrix(c(rep(1, n), invoiceData$x), ncol=2)

beta.hat = solve(t(X) %*% X) %*% (t(X) %*% Y); beta.hat

# OUTPUT:
#           [,1]
# [1,] 22.404762
# [2,]  3.619048

# Therefore the two methods are the same, we get the same regression line of
# Y-hat = 22.4048 + 3.618 x
#, where B0-hat = 22.4048, B1-hat = 3.618


# part c) finding 95% confidence interval for slope and intercept: 

# step 1: calculate Sxx:
x = invoiceData$x
Sxx = sum(x ^ 2) - (1/n) * sum(x)^2; Sxx
# 344.4

# Step 2: calculate Syy: 
Syy = sum(Y ^2) - (1/n) * sum(Y)^2; Syy
# 5626.9

# step 3: calculate SSE = Syy - B1-hat ^2 * Sxx
B1.hat = beta.hat[2]
SSE = Syy - B1.hat^2 * Sxx; SSE
# 1116.119

# step 4: s^2 = SSE / (n-2)
s = sqrt(SSE / (n-2)); s
# 11.81164


# Calculate critical t-value. There are df = n - 2 = 10 - 2 = 8
t.crit = abs(qt((1-0.95)/2, df=n-2)); t.crit
# 2.306004

# 95% Confidence interval for Intercept, B0: 
CI.B0 = beta.hat[1] + c(-1,1) * t.crit * s * sqrt(sum(x^2)/(n * Sxx))
CI.B0
# 0.9340886 43.8754352

# 95% Confidence interval for Slope: B1: 
CI.B1 = beta.hat[2] + c(-1,1) * t.crit * s * 1/sqrt(Sxx)
CI.B1
# 2.151343 5.086753
setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 3 - Linear Regression")
library(ISLR)
library(MASS)
library(car)

options(show.signif.stars = FALSE)

data(Auto)

head(Auto)

# a) 
auto.lm <- lm(mpg ~ horsepower, data=Auto)
summary(auto.lm)

# i) relation between predictor and response?
# CHECK: global F-test

# ANSWER: F-statistic is 599 and p value is near 0 so reject null hypothesis
# that the predictor coeffs are zero. Evidence of relation between predictor
# and the response
anova(auto.lm)


# ii) How strong is the relation between predictor and response?
# CHECK: s and R^2
summary(auto.lm)$sigma # s-squared is 4.905
summary(auto.lm)$r.squared # About 61% of variation in mpg is explained by horsepower
# so there is medium strength in relation between predictor and response

# iii) is relation between predictor positive or negative?
summary(auto.lm)
# NEgative because coefficient of horsepower < 0. As horsepower increases 1 unit, 
# the mpg decreases by 0.157 units. 

# iv) What is predicted mpg with horsepower of 98? and meanCi and predictCIs?
meanCI(auto.lm, x.values=98)
predictCI(auto.lm, x.values = 98)
# The fit is 24.46 mpg for horsepower of 98
# mean CI: we are 95% confident that for horsepower of 98, the mean mpg lies between
# 23.97 and 24.96.
# predict CI: we are 95% confident that for horsepower of 98, the future predicted
# value of mpg is between 14.809 and 34.124


# b) plot response and predictor
modelPlot(auto.lm)
# or old way
plot(x = Auto$horsepower, y = Auto$mpg)
abline(auto.lm, col="red")


# c) diagnostic plots

# 1) nonlinearity?
residualFitPlot(auto.lm)
# ISSUE: there is definite nonlinearity left in the data not captured by the
# simple regression line, shown by the quadratic shape in the residuals. 

# 1a) normality?
normalityPlot(auto.lm)
shapiro.test(auto.lm$residuals) # null hypo rejected: evidence of normality deviation

# 2) correlation of errors?
# not easy to check: errors must be independent by good experimental design.

# 3) heteroskedasticity: changing error variance with X?
residualFitPlot(auto.lm)
autoplot(auto.lm, which=3) # scale-location plot makes funnel more evident
# ISSUE: there is heteroskedasticity because there is some funnel shape
# in residuals, signals the variance of errors is not constant for all xs. 

# 4) outliers?
autoplot(auto.lm)
# ANy outlier has standardized residuals beyond (-2, 2)
auto.extra.lm <- fortify(auto.lm)
auto.extra.lm$.stdresid
# OR
s <- sqrt(summary(auto.lm)$sigma)
stdResid <- auto.lm$residuals / s
data.frame(r1=stdResid, r2=auto.extra.lm$.stdresid)
## HELP: why they are not the same?

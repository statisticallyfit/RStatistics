setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 3 - Linear Regression")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
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

# Using my outlier function: 
df.outliers <- outlier.outlierValues(auto.lm)
head(df.outliers)
which(df.outliers$IsOutlier)  # returns the observation values which are outliers
which.max(df.outliers$IsOutlier) # observation 116 has maximum outlier value


# 5)  LEVERAGE
df.cook <- influence.cooksDistances(auto.lm)
head(df.cook)
which(df.cook$IsInfluential) # no influential values based on cooks distance

df.lev <- influence.leverageValues(auto.lm)
head(df.lev)
head(df.cook)
which(df.lev$IsInfluential) # have tons of influential points based on leverage
which.max(df.lev$IsInfluential)


# 6) MULTICOLLINEARITY (only applies to a multiple regression)
cor(Auto[-c(1,7,8,9)])
# 0.84 = high correlation between horsepower and cylinders
# 0.95 = high cor between displacement and cylidners
# 0.89 = weight x cylinders
# -0.504 = acceleration, cylinders
# 0.897 = horsepower x displacement
# 0.93 = weight x displacement
# -0.54 = acceleration, displacement
# 0.86 = weight, horsepower
# -0.689 = acceleration, horsepower

# basically all the predictors are correlated!

auto.multiple.lm <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration,
                       data=Auto)
vif(auto.multiple.lm) # high vifs for all except for acceleration, so acceleration
# is not too highly correlated with other predictors on average. 

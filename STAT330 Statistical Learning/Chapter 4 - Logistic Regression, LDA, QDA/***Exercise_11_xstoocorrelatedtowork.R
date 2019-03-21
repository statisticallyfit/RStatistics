setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 4 - Logistic Regression, LDA, QDA/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')
library(ISLR)
library(MASS)
library(car)
library(caret)

options(show.signif.stars = FALSE)

data(Auto)

# GOAL: develop model to predict if given car gets high or low gas mileage

# part a)
medianMPG = median(Auto$mpg)
mpg01 <- rep(0, nrow(Auto))
mpg01[Auto$mpg > medianMPG] <- 1


autoData1 <- cbind(mpg01, Auto)
head(autoData1)


# part b) explore data graphically  - association between mpg01 and other predictors
cor(autoData1[,-c(10)])
pairs(autoData1[,-c(10)])

# High correlations everywhere
# The only low correlations are: 
# 0.346: acceleration, mpg01
# 0.29: year, acceleration
# 0.21: acceleration, origin
# 0.18, year, origin

# High correlations between displacement, horsepower, weight, cylinders
# But also mpg has high correlations individually with each of these , suggesting
# that mpg should be predicted using each of these (maybe separately?)


# boxplot
autoData <- autoData1
autoData$mpg01 <- factor(autoData$mpg01)

# mpg01 vs cylinders --- YES
ggplot(data=autoData, aes(x=mpg01, y=cylinders, group=mpg01,color=mpg01)) + geom_boxplot()
# For values of mpg below its median, cylinders are very high and have large IQR (variation)
# but have almost no variation for values of mpg above its median, and cylinders are also
# very low here. 
cor(autoData$mpg, autoData$cylinders)

# mpg01 vs horsepower --- YES, high cor
ggplot(autoData, aes(x=mpg01, y=horsepower, color=mpg01)) + geom_boxplot()
cor(autoData$mpg, autoData$horsepower)

# mpg01 vs acceleration ---- false alarm: NOT MUCH PREDICTION POWER
ggplot(autoData, aes(x=mpg01, y=acceleration, color=mpg01)) + geom_boxplot()
cor(autoData$mpg, autoData$acceleration)

# mpg01 vs displacement --- YES
ggplot(autoData, aes(x=mpg01, y=displacement, color=mpg01)) + geom_boxplot()
cor(autoData$mpg, autoData$displacement)

# mpg01 vs weight --- YES
ggplot(autoData, aes(x=mpg01, y=weight, color=mpg01)) + geom_boxplot()
cor(autoData$mpg, autoData$weight)


cor(autoData1[,-10])
# mpg and mpg01 both each have high correlations with:
# displacement, horsepower, weight
# so that is why we choose them as predictors


# part c) ------ train vs test set
trainIndices = autoData$year %% 2 # even years are training
trainData <- autoData[trainIndices, ]
testData <- autoData[!trainIndices, ]



# part d) ---- lda on training to predict mpg01, find test error
auto.lda <- lda(mpg01 ~ horsepower, data=trainData)


# part e) --- qda ------------------------


# part f) --- logistic------------------------
auto.glm <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, data=trainData, family=binomial)
# algo did not converge

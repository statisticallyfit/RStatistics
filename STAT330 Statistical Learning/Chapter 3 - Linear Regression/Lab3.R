setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 3 - Linear Regression")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
library(ISLR)
library(MASS)
library(car)

options(show.signif.stars = FALSE)

# fix(Boston)
head(Boston)
names(Boston)

boston.lm <- lm(medv ~ lstat, data=Boston)
summary(boston.lm)

# confidence intervals for COEFFICIENTS
slopeCI(boston.lm)
confint(boston.lm)

# meanCI (confidence intervals for average Y)
meanCI(boston.lm, x.values=c(5, 10, 15)) # different confints for lstat = 5, lstat = 10
# and lstat = 15
# Same thing as: 
predict(boston.lm, newdata = data.frame(lstat=c(5, 10, 15)), interval="conf")

# INTERPRET: We are 95% confident that the mean value of "medv" is between 
# (29.007, 30.6) for all predictors (just lstat) at value lstat = 5

# Prediction intervals to predict the single future y = medv for given value of lstat
predictCI(boston.lm, x.values = c(5,10,15))
predict(boston.lm, newdata = data.frame(lstat=c(5, 10, 15)), interval="pred")


# Plot the medv and lstat and least squares line
modelPlot(boston.lm)

plotConfidenceBands.lm(boston.lm)
plotConfPredBands.lm(boston.lm)

residualFitPlot(boston.lm)
normalityPlot(boston.lm)

par(mfrow=c(1,2))
plot(boston.lm, which=c(1,2))
plot(boston.lm, which=c(3,5), cook.levels = c(0.2, 0.5, 1))

# OR all together
par(mfrow=c(2,2))
plot(boston.lm)
# all together with ggplot
autoplot(boston.lm)
autoplot(boston.lm, which=c(1,2))
autoplot(boston.lm, which=c(3,4),size=2)
cooksPlot(boston.lm)
autoplot(boston.lm, which=c(5,6),size=2)

# ISSUE 1: evidence of nonlinearity because scale-location and residual plots
# have significant curvature

# checking normality
shapiro.test(boston.lm$residuals)
normalityPlot(boston.lm)

# ISSUE 2: evidence of non normality because null hypothesis of normal data
# is rejected, and qq plot veers off at the edges. 

levInfo <- influence.leverageValues(boston.lm)
sum(levInfo$IsInfluential) # there are 34 influential points
which.max(levInfo$InfluentialPoints) # which observation has max leverage

cookInfo <- influence.cooksDistances(boston.lm)
which(cookInfo$IsInfluential)
which.max(cookInfo$CooksPoints) # observation 375 also has high cook value


# ISSUE 3: high outliers and leverages



# -------------------------------------
# Multiple regression
boston2.lm <- lm(medv ~ lstat + age, data=Boston)
summary(boston2.lm)
vif(boston2.lm) # no evidence of multicollinearity

# on all predictors
boston.all.lm <- lm(medv ~ ., data=Boston)
summary(boston.all.lm)
vif(boston.all.lm) # some evidence of multicollinearity between TAX and other predictors
# since tax has VIF close to 10

summary(boston.all.lm)$sigma # is the RSE (or s = standard error of regression line)

# regression of all vars except age
boston.notage.lm <- lm(medv ~ . -age, data=Boston)
summary(boston.notage.lm)

# Or use update
boston.notage.update.lm <- update(boston.all.lm, ~. -age)
summary(boston.notage.update.lm)


# Interaction terms ------------------------------------------------------
# lstat:age = interaction term for lstat and age
# lstat * age = shorthand for lstat + age + lstat:age (so includes main effects)

summary(lm(medv ~ lstat * age, data=Boston)) # evidence of significant interaction
# between lstat and age p - value = 0.025

# POLYNOMIAL (NONLINEAR) TRANSFORMATIONS OF PREDICTORS ---------------------------------------
# I(X^2) for predictor X

boston.square.lm <- lm(medv ~ lstat + I(lstat^2), data=Boston)
summary(boston.square.lm)

# anova to quantify how much the quadratic fit is superior to linear fit
anova(boston.lm, boston.square.lm)
NestedFTest(boston.lm, boston.square.lm)

# null hypothesis: the models fit data equally well.
# alternative: full model is superior (squared term model)
# p-value is 0 so H0 is rejected and full model is better. 

autoplot(boston.square.lm) # less curvature in residuals than for linear model


# fifth order poly fit
#boston.poly5.lm <- lm(medv ~ poly(lstat, 5), data=Boston)
boston.poly5.lm <- lm(medv ~ lstat + I(lstat^2) + I(lstat^3) + I(lstat^4)
                      + I(lstat^5), data=Boston)
summary(boston.poly5.lm)

anova(boston.lm, boston.poly5.lm)
NestedFTest(boston.lm, boston.poly5.lm) # fifth order model is better

autoplot(boston.poly5.lm)

# any poly terms beyond fifth order have no more significant p-values
boston.poly6.lm <- lm(medv ~ poly(lstat, 6), data=Boston)
anova(boston.poly5.lm, boston.poly6.lm) # no significant improvement between 5,6
anova(boston.lm, boston.poly6.lm)
summary(boston.poly6.lm) # now sixth order term not significant. 


# LOG predictor model -----------------------------------------
summary(lm(medv ~ log(rm), data=Boston))



# QUALITATIVE PREDICTORS ------------------------------------------------------
data("Carseats")
head(Carseats)

# all the vars, plus these interactions
carseats.interact.lm <- lm(Sales ~ . + Income:Advertising + Price:Age, data=Carseats)
summary(carseats.interact.lm)

# NOTE: ShelvLocGood coeff is positive => so mean sales is higher for good loc
# and lower for bad loc (base variable). 
# ShelveLocMedium coeff is positive but loer than Good => so means sales is higher
# for med loc than for bad loc, but difference is less than between good and bad loc.

# contrasts returns coding for variables
contrasts(Carseats$ShelveLoc)

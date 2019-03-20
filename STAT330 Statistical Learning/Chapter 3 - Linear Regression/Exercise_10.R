setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/Chapter 3 - Linear Regression")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
library(ISLR)
library(MASS)
library(car)

options(show.signif.stars = FALSE)

data(Carseats)

# a) 
head(Carseats)
car.lm <- lm(Sales ~ Price + Urban + US, data=Carseats)

# b) coef interpretation:
summary(car.lm)
# Price: for each 1 unit increase in price, sales is expected to decrease by 0.05 units,
#holding other predictors fixed
# URBAN_YES = sales is lower (nonsig) for urban=yes than for urban=no
# US_YES => sales is higher (sig) for ys=yes than for us=no

# d) can reject null for price and US+yes, not urban

# e) fit smaller model
car.lm2 <- lm(Sales ~ Price + US, data=Carseats)
summary(car.lm2)

# f) Data fit
summary(car.lm2)$r.sq
summary(car.lm)$r.sq

# both R-squared are small so they fit data poorly
# But F=statistic (global test) is significant for both

# g) confidence intervals for coefficients of slope

betaCI(car.lm2)
betaCI(car.lm)

# h)
df.out <- outlier.outlierValues(car.lm2)
head(df.out)
which(df.out$IsOutlier)
which.max(df.out$IsOutlier)
# many outliers

df.cook <- influence.cooksDistances(car.lm2)
which(df.cook$IsInfluential) # no cooks leverage points

df.lev <- influence.leverageValues(car.lm2)
head(df.lev)
which(df.lev$IsInfluential) # many influential points
which(df.lev$InfluentialPoints > df.lev$AvgLev) # influentials by average leverage criteria
which.max(df.lev$InfluentialPoints > df.lev$AvgLev)

autoplot(car.lm2)
shapiro.test(car.lm2$residuals)
# residuals - good no pattern
# normal data qq plot
# but just have some residuals and leverage
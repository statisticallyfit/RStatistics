setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(MASS) 

options(show.signif.stars = FALSE)

jawData <- read.table("data/jaws.txt", header=TRUE)
head(jawData)

# Exploratory
ggplot(jawData, aes(x=age, y=bone)) + geom_point()


# Nonlinear model: y = a - b*exp(-c * age)

# Estimate (a): 
# --> Asumptote is (a): Take age -> infinity then the bexp(age) term goes to zero so only (a)
# is left so we see the asymptote on the graph is  aroudn 120 (horizontal line for age -> infinity)
a0 <- 120

# Estimate (b): 
# --> Consider intercept : age = 0. Then y = a - b, where y = intercept. 
# Book says intercept is 10 (???) So then: 10 = 120 - b ==> b = 110
b0 <- 110

# Estimate (c): 
# y = a - b*exp(-c * age) ===>    c = (log(b) - log(a-y)) / x
# Using age = 5, y = 40 by plot inspection: 
c0 = (log(b0) - log(a0-40)) / 5; c0


jaw.nls <- nls(bone ~ a - b*exp(-c * age), start=list(a=a0, b = b0, c = c0), data=jawData)
summary(jaw.nls)
# NOTE: even if all parameters are significant it doesn't mean they all need
# to be retained in the model. The parameters a = 115, b = 118 are too close (and would
# need to differ by more than 2 standard errors to be significant, in general)


# Fit the simpler model 2-parameter model: y = a(1 - exp(-c * age))
jaw2.nls <- nls(bone ~ a * (1 - exp(-c*age)), start=list(a = a0, c = c0), data=jawData)
summary(jaw2.nls)

anova(jaw2.nls, jaw.nls)
# INTERPRET: the extra term (b) was not significant. So accept two-parameter model. 



# Plot the predicions through the scatterplot. 
ageValues <- seq(0, 50, 0.1)
pred <- predict(jaw2.nls, list(age = ageValues))
pred.df <- data.frame(age=ageValues, pred = pred)

ggplot(data=jawData, aes(x=age, y=bone)) + geom_point() + 
      geom_line(data=pred.df, aes(y = pred), color="dodgerblue", size=2)


# Calculation of R^2 (variation explained)
df.residual <- summary(jaw2.nls)$df[2]
df.residual
s <- summary(jaw2.nls)$sigma # called the standard error of the regr model
s # residual standard error
SSE <- s^2 * df.residual
SSE

# Fit the null model to get the total SST
jawNull.lm <- lm(bone ~ 1, data=jawData)
df.total <- summary(jawNull.lm)$df[2]; df.total
s <- summary(jawNull.lm)$sigma
SST <- s^2 * df.total
SST

# R squared is just: percentage variation
R2 <- 1 - SSE/SST; R2

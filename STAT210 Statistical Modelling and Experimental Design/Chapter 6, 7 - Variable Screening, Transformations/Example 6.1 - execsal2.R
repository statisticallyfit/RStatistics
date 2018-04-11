setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/EXEXSAL2.Rdata")

options(digits=10)

fit.all <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, 
              data=EXEXSAL2)


fit.start <- lm(Y ~ 1, data=EXEXSAL2) # intercept-only model

# <none> means add nothing to intercept-only model. 
# compare each of the AICs per box with the none row in that box. 

# FORWARD: means don't recheck t-values at eachs tage. 
stepwise.forward <- step(fit.start, direction="forward", scope=formula(fit.all), test="F")
summary(stepwise.forward)

# BOTH is default - meaning? Probably rechecks at each stage. 
stepwise.both <- step(fit.start, direction="both", scope=formula(fit.all), test="F")
summary(stepwise.both)


# BACKWARD: means fit ALL the terms then drop predictor with lowest t-value. 
# Then re-fit remaining terms and drop lowest t-value. Repeat until no
# more nonsignificant predictors.
stepwise.back <- step(fit.all, direction="backward", test="F")
summary(stepwise.back)

formula(stepwise.both)
formula(stepwise.forward)
formula(stepwise.back)

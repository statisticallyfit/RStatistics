setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/SOCWORK.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)


# Fitting the quadratic model - see we will still see a cone shape
# in the residual vs fitted plot, signifiying the model is actually
# multiplicative and we must regress ln(y) instead of y
social.lm <- lm(SALARY ~ EXP, data=SOCWORK)
residualFittedPlot(social.lm) # cone + curve shape
partialPlot(social.lm, variableName = "EXP") # indicates we should use quadratic

# model 2 quadratic
social.quad.lm <- lm(SALARY ~ EXP + I(EXP^2), data=SOCWORK)
residualFittedPlot(social.quad.lm) # now just pure cone shape, no more curve
# quadratic term was successful. Curve in residuals is eliminated.
partialPlot(social.quad.lm, variableName = "I(EXP^2)")
partialPlot(social.quad.lm, variableName = "EXP")


# Now fitting the multiplicative model ln(y)
social.log.lm <- lm(LNSALARY ~ EXP + I(EXP^2), data=SOCWORK)
summary(social.log.lm)
residualFittedPlot(social.log.lm) # no more pattern indicative of non-const variance
# of errors (residuals)
partialPlot(social.log.lm, variableName = "EXP")
partialPlot(social.log.lm, variableName = "I(EXP^2)")


# But in the ln-quadratic model the quadratic term is not significant. 
# Can remove it once ln is included in the model
social.log.noquad.lm <- lm(LNSALARY ~ EXP, data=SOCWORK)
summary(social.log.noquad.lm)

residualFittedPlot(social.log.noquad.lm)
partialPlot(social.log.noquad.lm, variableName = "EXP")

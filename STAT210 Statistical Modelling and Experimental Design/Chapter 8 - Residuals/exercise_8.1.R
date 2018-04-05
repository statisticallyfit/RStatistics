setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/EX8_1.Rdata")
library(ggplot2)
library(ggfortify)


yhat <- lm(Y ~ X, data=EX8_1)
summary(yhat)
yhat$residuals

residualFittedPlot(yhat)
normalQQPlot(yhat)
autoplot(yhat)
# Pattern suggests curvilinear term is necessary, but residual plot
# does not specify which type of curvilinearity exactly (quadratic, inverse ...)


# now partial residuals
partialPlot(yhat, variableName = "X")


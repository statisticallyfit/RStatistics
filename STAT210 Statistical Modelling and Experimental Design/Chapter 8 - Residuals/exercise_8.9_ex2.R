setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/EX8_2.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)


yhat <- lm(Y ~ X, data=EX8_2)
residualFittedPlot(yhat)
# constant variance is satisfied since resids don't change as fitteds change
# but there is a curvature trend so must use curvature term
partialPlot(yhat, variableName = "X")

setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/ASWELLS.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)

arsenic.lm <- lm(ARSENIC ~ LATITUDE + LONGITUDE + DEPTHFT, data=ASWELLS)


residualFittedPlot(arsenic.lm) # nope, non-const variance!!!

normalQQPlot(arsenic.lm) # nope, deviates strongly from normality
shapiro.test(arsenic.lm$residuals) # reject H0 of normality. 
cooks.distance(arsenic.lm)

partialPlot(arsenic.lm, variableName = "LATITUDE")
partialPlot(arsenic.lm, variableName = "LONGITUDE")
partialPlot(arsenic.lm, variableName = "DEPTHFT")



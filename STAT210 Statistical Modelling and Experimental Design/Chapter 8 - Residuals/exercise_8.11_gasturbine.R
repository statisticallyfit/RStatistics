setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/GASTURBINE.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)


gas.lm <- lm(HEATRATE ~ RPM + CPRATIO + RPM:CPRATIO, data=GASTURBINE)

residualFittedPlot(gas.lm)
normalQQPlot(gas.lm) # not that normal

shapiro.test(gas.lm$residuals)# reject null 


partialPlot(gas.lm, variableName = "RPM")
partialPlot(gas.lm, variableName = "CPRATIO")

summary(gas.lm)

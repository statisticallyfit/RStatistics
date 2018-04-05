setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/APPLIANCE.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)

appliance.lm <- lm(BUYPROP ~ AGE, data=APPLIANCE)
summary(appliance.lm)

residualFittedPlot(appliance.lm)
partialPlot(appliance.lm, variableName = "AGE")


# stabilized binomial model with appropriate transformation
appliance.arcsin.lm <- lm(asin(sqrt(BUYPROP)) ~ AGE, data=APPLIANCE)
summary(appliance.arcsin.lm)

residualFittedPlot(appliance.arcsin.lm)

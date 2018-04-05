setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

library(ggplot2)
library(car)
load("data/Exercises and Examples/COFFEE2.Rdata")
is.factor(COFFEE2$X)
COFFEE2$X <- as.factor(COFFEE2$X)

# note: X = adverteizemen
coffeeDemand.lm <- lm(DEMAND ~ PRICE + X, data=COFFEE2)

summary(coffeeDemand.lm)

# partial plotting
partialPlot(coffeeDemand.lm, variableName = "PRICE")

crPlot(coffeeDemand.lm, variable="PRICE")



# Transformed model with 1 / PRICE
coffeeDemandInversePrice.lm <- lm(DEMAND ~ I(1/PRICE) + X, data=COFFEE2)
summary(coffeeDemandInversePrice.lm)

partialPlot(coffeeDemandInversePrice.lm, variableName = "I(1/PRICE)")


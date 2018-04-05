setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/TIRES.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)


# Straight line model
tire.lm <- lm(MILEAGE_Y ~ PRESS_X, data=TIRES)
summary(tire.lm)


autoplot(tire.lm, which=1, colour = "deeppink", size=4)

partialPlot(tire.lm, variableName = "PRESS_X", size=5, colour="royalblue")


# Quadratic model
tire.quad.lm <- lm(MILEAGE_Y ~ PRESS_X + I(PRESS_X^2), data=TIRES)
summary(tire.quad.lm)

partialPlot(tire.quad.lm, variableName = "PRESS_X")
partialPlot(tire.quad.lm, variableName = "I(PRESS_X^2)")

# regular residuals
autoplot(tire.quad.lm, which=1, size=4) # quadratic term made pattern vanish. 

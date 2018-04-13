setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R")

load("data/Exercises and Examples/ASWELLS.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)

arsenic.lm <- lm(ARSENIC ~ LATITUDE + LONGITUDE + DEPTHFT, data=ASWELLS)
betaCI(arsenic.lm) # confints are all not containing zero. 


residualFitPlot(arsenic.lm) # nope, non-const variance!!!
normalityPlot(arsenic.lm) # nope, deviates strongly from normality
shapiro.test(arsenic.lm$residuals) # reject H0 of normality. 
length(which(cooksDistance(arsenic.lm)$IsInfl == TRUE)) # none are influential

partialPlot(arsenic.lm, variableName = "LATITUDE")
partialPlot(arsenic.lm, variableName = "LONGITUDE")
partialPlot(arsenic.lm, variableName = "DEPTHFT")




# FINDING CORRECT POWER: 
#library(MASS)
#boxcox(ARSENIC ~ LATITUDE + LONGITUDE + DEPTHFT, data=ASWELLS, 
#       lambda=seq(0, 1, by=0.01)) # sees lambda = 0.2
library(lindia)
gg_boxcox(arsenic.lm)

arsenic.power.lm <- lm(ARSENIC ^0.2~ LATITUDE + LONGITUDE + DEPTHFT, data=ASWELLS)
anova(arsenic.power.lm)

normalityPlot(arsenic.power.lm) # much straighter
residualFitPlot(arsenic.power.lm) # but not good still. 

# WE can backtransform to get results back in orginainal form. 
arsenic.power.lm$fitted.values^(1/0.2)

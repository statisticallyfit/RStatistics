setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/BRAINPAIN.Rdata")

reg.line <- lm(ACTIVITY ~ EMPATHY, data=BRAINPAIN)
summary(reg.line)
reg.line$model

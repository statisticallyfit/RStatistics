setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/CLERICAL.Rdata")
options(digits=10)

clerical.all <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, data=CLERICAL)
summary(clerical.all)

clerical.start <- lm(Y ~ 1, data=CLERICAL)

clerical.stepwise.both <- step(clerical.start, scope=formula(clerical.all), test="F")
formula(clerical.stepwise.both)
summary(clerical.stepwise.both)


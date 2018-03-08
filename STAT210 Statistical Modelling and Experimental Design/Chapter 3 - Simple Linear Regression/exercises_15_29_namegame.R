setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/NAMEGAME2.Rdata")

reg.line <- lm(RECALL ~ POSITION, data=NAMEGAME2)
summary(reg.line)


setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/ACCHW.Rdata")

account.lm <- lm(IMPROVE ~ ASSIST, data=ACCHW)
summary(account.lm)

anova(account.lm)

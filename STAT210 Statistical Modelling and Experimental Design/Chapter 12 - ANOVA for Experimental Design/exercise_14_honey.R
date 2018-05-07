setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/HONEYCOUGH.Rdata")

honey.lm <- lm(TOTSCORE ~ TREATMENT, data=HONEYCOUGH)
summary(honey.lm)

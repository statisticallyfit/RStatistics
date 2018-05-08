setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/PRACEXAM.Rdata")
options(digits=10, show.signif.stars = FALSE)

exam.lm <- lm(RATING ~ PREP*STANDING, data=PRACEXAM)
anova(exam.lm)

# interaction not signif, prep signif, standing not signif. 
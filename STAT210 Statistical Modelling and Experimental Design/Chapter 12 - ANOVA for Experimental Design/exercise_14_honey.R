setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/HONEYCOUGH.Rdata")

subset(HONEYCOUGH, TREATMENT=="H")
honeyData <- removeWhitespace(HONEYCOUGH, colsToFix = "TREATMENT")
is.factor(honeyData$TOTSCORE)
is.factor(honeyData$TREATMENT)


honey.lm <- lm(TOTSCORE ~ TREATMENT, data=honeyData)
summary(honey.lm)


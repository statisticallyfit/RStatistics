setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/DRINKERS.Rdata")


# complete randomized design model
drinkers.lm <- lm(SCORE ~ GROUP, data=DRINKERS)
summary(drinkers.lm)

# F p-value is low so reject H0 that means are the same - conclude
# at least 2 means differ. (the global F-test is the same thing as testing
# that means are the same for a complete randomized design)

setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/FORMULAS.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/INTERPRET.R', echo=FALSE)

load("data/Exercises and Examples/GFCLOCKS.Rdata")

# assume interaction term: age * price - rate of increase of price with age 
# is driven upward with number of bidders. 
clock.interact.lm <- lm(PRICE ~ AGE + NUMBIDS + AGE_BID, data=GFCLOCKS)
summary(clock.interact.lm)

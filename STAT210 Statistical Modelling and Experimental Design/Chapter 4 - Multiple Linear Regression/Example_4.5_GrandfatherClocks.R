setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/GFCLOCKS.Rdata")


clock.lm <- lm(PRICE ~ AGE + NUMBIDS, data=GFCLOCKS)
summary(clock.lm)

# part a)
meanCI(clock.lm, x.values=c(150, 10))
interpret.MeanCI(clock.lm, x.values=c(150,10), x.units=c("year", "bidders"))

# part b)
predictCI(clock.lm, x.values=c(150, 10))
interpret.PredictCI(clock.lm, x.values=c(150,10), x.units=c("year", "bidders"))

# part c)
predictCI(clock.lm, x.values=c(50, 2))
# but we know this won't make sense because the values don't fall in the ranges: 
range(GFCLOCKS$AGE)
range(GFCLOCKS$NUMBIDS)



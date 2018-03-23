setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/MTBE.Rdata")
options(digits=10)

names(MTBE)

mtbe.all <- lm(MTBE ~ WellClass + Aquifier + pH + Depth + DissOxy + Distance + 
                     IndPct, data=na.omit(MTBE))
summary(mtbe.all)

mtbe.start <- lm(MTBE ~ 1, data=na.omit(MTBE))

# TODO what is going on why error? 
mtbe.stepwise <- step(mtbe.start, scope=formula(mtbe.all), test="F")
formula(mtbe.stepwise)
summary(mtbe.stepwise)

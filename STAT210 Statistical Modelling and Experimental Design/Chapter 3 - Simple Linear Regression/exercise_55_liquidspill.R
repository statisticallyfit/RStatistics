setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/LIQUIDSPILL.Rdata")

reg.line <- lm(MASS ~ TIME, data=LIQUIDSPILL)
summary(reg.line)

y <- LIQUIDSPILL$MASS
x <- LIQUIDSPILL$TIME 
yhat <- reg.line$fitted.values

meanCI(x, y, reg.line, x.value=15, level=0.90)
interpret.MeanCI(x, y, reg.line, x.value=15, level=0.90, x.unit="pounds", 
                    x.name="mass", y.unit="minutes",y.name="time")

predictCI(x, y, reg.line, x.value=15, level=0.90)
interpret.PredictCI(x, y, reg.line, x.value=15, level=0.90, x.unit="pounds", 
                    x.name="mass", y.unit="minutes",y.name="time")

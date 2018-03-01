setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/WHITESPRUCE.Rdata")

reg.line <- lm(HEIGHT ~ DIAMETER, data=WHITESPRUCE)
summary(reg.line)

y <- WHITESPRUCE$HEIGHT
x <- WHITESPRUCE$DIAMETER
yhat <- reg.line$fitted.values

meanCI(x, y, reg.line, x.value=20, level=0.90)
interpret.MeanCI(x, y, reg.line, x.value=20, level=0.90, x.unit="cm", 
                 x.name="breast height diameter", y.unit="m",y.name="height")

predictCI(x, y, reg.line, x.value=20, level=0.90)
interpret.PredictCI(x, y, reg.line, x.value=20, level=0.90, x.unit="cm", 
                    x.name="breast height diameter", y.unit="m",y.name="height")

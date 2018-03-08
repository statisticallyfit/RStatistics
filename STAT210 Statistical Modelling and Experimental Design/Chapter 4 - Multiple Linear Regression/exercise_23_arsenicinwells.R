setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/ASWELLS.Rdata")

arsenic.lm <- lm(ARSENIC ~ LATITUDE + LONGITUDE + DEPTHFT, data=ASWELLS)
summary(arsenic.lm)

interpret.SlopeCoeffs(arsenic.lm)
interpret.SlopeCI(arsenic.lm, x.units = c("degrees", "degrees", "feet"))


# 
lowestLatitude <- range(ASWELLS$LATITUDE)[1]; lowestLatitude
highestLongitude <- range(ASWELLS$LONGITUDE)[2]; highestLongitude
lowestDepth <- range(ASWELLS$DEPTHFT, na.rm=TRUE)[1]; lowestDepth
x.values <- c(lowestLatitude, highestLongitude, lowestDepth)

meanCI(arsenic.lm, x.values=x.values)
interpret.MeanCI(arsenic.lm, x.values=x.values, x.units=c("degrees", "degrees", "feet"))

predictCI(arsenic.lm, x.values=x.values)
interpret.PredictCI(arsenic.lm, x.values = x.values, x.units = c("degrees","degrees","feet"))


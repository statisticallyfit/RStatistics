setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/BOILERS.Rdata")
library(ggplot2)
library(ggfortify)
options(digits=10)

attach(BOILERS)
boiler.lm <- lm(MANHRS ~ CAPACITY + PRESSURE + BOILER + DRUM, data=BOILERS)
summary(boiler.lm)

boiler.lm$residuals

# plotting resdiauls versus x1 (not a partial plot)
df <- data.frame(X1=BOILERS$CAPACITY, X2=BOILERS$PRESSURE, 
                 X3=BOILERS$BOILER, X4=BOILERS$DRUM, Resids=boiler.lm$residuals)
ggplot(df, aes(x=X1, y=Resids)) + geom_point()
# variance seems variabile
ggplot(df, aes(x=X2, y=Resids)) + geom_point()
# no trends either
ggplot(df, aes(x=X3, y=Resids)) + geom_point()
ggplot(df, aes(x=X4, y=Resids)) + geom_point()
# both dummies seem well distributed around the y = hat line though there
# is some skewness (?)

residualPlot(boiler.lm, variableName = "CAPACITY")

# partial plots - all is well, no trend for either of them. 
partialPlot(boiler.lm, variableName = "CAPACITY")
partialPlot(boiler.lm, variableName = "PRESSURE")



# fitting interaction model between capacity and pressure
boiler.interact.lm <- update(boiler.lm, . ~ . + CAP_PRESS, data=BOILERS)
# NOTE: not supposed to work in plotting since vars are continuous
interaction.plot(x.factor=CAPACITY, trace.factor = PRESSURE, response=MANHRS)
interactionPlot(data=BOILERS, xFactor="CAPACITY",traceFactor="PRESSURE",response="MANHRS")

summary(boiler.interact.lm)

residualFittedPlot(boiler.interact.lm)
partialPlot(boiler.interact.lm, variableName = "CAP_PRESS")

detach(BOILERS)

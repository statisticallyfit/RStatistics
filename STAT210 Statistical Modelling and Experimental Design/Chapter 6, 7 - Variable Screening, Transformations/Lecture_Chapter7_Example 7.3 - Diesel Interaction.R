setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/DIESEL.Rdata")
load("data/Exercises and Examples/DIELSEL2.Rdata")
library(ggplot2)


options(digits=10, show.signif.stars = FALSE)

diesel.lm <- lm(PERFORM ~ BRAND + FUEL, data=DIESEL)
summary(diesel.lm)



# PLOTTING INTERACTION
attach(DIESEL)

# we can see fthat for different levels of Fuel (F1, F2, F3), there
# are non-parallel brand curves of performance. Interaction!

# METHOD 1
interactionPlot(data=DIESEL, xFactor = "BRAND", traceFactor = "FUEL", 
                response="PERFORM")

# METHOD 2
interaction.plot(x.factor = BRAND, trace.factor = FUEL, response = PERFORM)

args(interaction.plot)

detach(DIESEL)


# === Now for when data is missing - when any data is missing, ALL
# interaction term coefficients are missing!
# No data for F3B1 and F1B2
attach(DIELSEL2)

diesel.MISSING.XS.lm <-lm(PERFORM ~ X1 + X2 + X3 + X1X3 + X2X3, data=DIELSEL2)

diesel.MISSING.lm <- lm(PERFORM ~ FUEL + BRAND + FUEL:BRAND, data=DIELSEL2)

summary(diesel.MISSING.XS.lm)
summary(diesel.MISSING.lm) # see two parameters are missing, the interactions. 
summary(diesel.lm)


anova(diesel.MISSING.lm)
# NOTE: Fuel has 3 levels do df = 2, and BRAND has 2 levels so df = 1
nrow(DIELSEL2)



# we can see fthat for different levels of Fuel (F1, F2, F3), there
# are non-parallel brand curves of performance. Interaction!

# METHOD 1 (not enough data for this dataset)
interactionPlot(data=DIELSEL2, xFactor = "BRAND", response="PERFORM", traceFactor="FUEL")

# METHOD 2 (not enough data for this dataset)
interaction.plot(x.factor = BRAND, trace.factor = FUEL, response = PERFORM)


detach(DIELSEL2)

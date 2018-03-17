setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/PRODQUAL.Rdata")
library(ggplot2)
attach(PRODQUAL)

# Graphing plots
# x1 = temperature (fahrenheit)
# x2 = pressure (pounds per square inch)
# y = quality of finished product
g1 <- ggplot(PRODQUAL, aes(x = TEMP, y = QUALITY)) + 
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x = "Temperature (F)", y = "Quality")
g1 #

g2 <- ggplot(PRODQUAL, aes(x = PRESSURE, y = QUALITY)) + 
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x = "Pressure (psi)", y = "Quality")
g2 #


# Fitting complete second-order model
# E(y) = B0 + B1x1 + B2x2 + B3x1x2 + B4x1^2 + B5x2^2
prodqual.lm <- lm(QUALITY ~ TEMP + PRESSURE + TEMP*PRESSURE
                  + I(TEMP^2) + I(PRESSURE^2), data=PRODQUAL)
summary(prodqual.lm)


# Testing whether the B4 B4 quadratic terms are significant (nest F-test)
reduced.lm <- lm(QUALITY ~ TEMP + PRESSURE + TEMP*PRESSURE, data=PRODQUAL)
NestedFandChiSqTest(reduced.lm, prodqual.lm)

anova(reduced.lm, prodqual.lm)

detach(PRODQUAL)

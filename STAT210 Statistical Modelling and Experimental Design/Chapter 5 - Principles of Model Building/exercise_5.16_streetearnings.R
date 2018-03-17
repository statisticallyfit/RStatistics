setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/FORMULAS.R',echo=FALSE)

load("data/Exercises and Examples/STREETVN.Rdata")
attach(STREETVN)

library(ggplot2)

# Graphing plots
g1 <- ggplot(STREETVN, aes(x = AGE, y = EARNINGS)) + 
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x = "Age", y = "Earnings")
g1 #
g2 <- ggplot(STREETVN, aes(x = HOURS, y = EARNINGS)) + 
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x = "Age", y = "Earnings")
g2 #


# Fitting complete second order model
street.lm <- lm(EARNINGS ~ AGE + HOURS + AGE*HOURS + I(AGE^2) + I(HOURS^2),
                data=STREETVN)
summary(street.lm)
reduced.lm <- lm(EARNINGS ~ AGE + HOURS, data=STREETVN)
summary(reduced.lm)

anova(reduced.lm, street.lm)
NestedFandChiSqTest(reduced.lm, street.lm)

detach(STREETVN)

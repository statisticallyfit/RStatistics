setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R',echo=FALSE)

load("data/Exercises and Examples/QUASAR.Rdata")
attach(QUASAR)

library(ggplot2)

# Graphing plots
# x1 = redshift
# x2 = lineflux
# x4 = AB1450
g1 <- ggplot(QUASAR, aes(x = REDSHIFT, y = RFEWIDTH)) + 
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x = "Redshift", y = "Equivalent Width")
g1 #
g2 <- ggplot(QUASAR, aes(x = LINEFLUX, y = RFEWIDTH)) + 
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x = "Lineflux", y = "Equivalent Width")
g2 #
g3 <- ggplot(QUASAR, aes(x = AB1450, y = RFEWIDTH)) + 
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x = "AB1450", y = "Equivalent Width")
g3 #


# Fitting complete second order model
quasar.lm <- lm(RFEWIDTH ~ REDSHIFT + LINEFLUX + AB1450 + REDSHIFT*LINEFLUX +
                      LINEFLUX*AB1450 + REDSHIFT*AB1450 + I(REDSHIFT^2) +
                      I(LINEFLUX^2) + I(AB1450^2), data=QUASAR)
summary(quasar.lm)

reduced.lm <- lm(RFEWIDTH ~ REDSHIFT + LINEFLUX + AB1450, data=QUASAR)
summary(reduced.lm)
n = nrow(QUASAR); n #

anova(reduced.lm, quasar.lm)
NestedFandChiSqTest(reduced.lm, quasar.lm)

detach(QUASAR)
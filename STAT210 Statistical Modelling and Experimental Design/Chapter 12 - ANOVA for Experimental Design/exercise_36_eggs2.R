setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/EGGS2.Rdata")
options(digits=10, show.signif.stars = FALSE)


egg.lm <- lm(OVERRUN ~ HOUSING + WT + HOUSING:WT, data =EGGS2)
summary(egg.lm)

# testing interaction: p = 0.51 > 0.05 = so not significant
# testing main effects = WT not significant, but HOUSING is, so means differ
# among different housing but not for different weights. 
anova(egg.lm)

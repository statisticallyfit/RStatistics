setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A2/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')

library(ggplot2)
library(lattice)
options(digits=10, show.signif.stars = F)


data("ToothGrowth")
ToothGrowth$dose <- factor(ToothGrowth$dose)
attach(ToothGrowth)


# part b)
interaction.plot(x.factor=dose, trace.factor = supp, response=len)


tooth.interact.lm <- lm(len ~ dose * supp, data=ToothGrowth)
anova(tooth.interact.lm)
# the interaction term is significant so we keep it. 
summary(tooth.interact.lm)

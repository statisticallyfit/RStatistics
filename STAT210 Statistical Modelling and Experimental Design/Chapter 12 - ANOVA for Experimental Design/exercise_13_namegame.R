setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/NAMEGAME.Rdata")


# 1 =  simple name game, 2 = elaborate name game, 3 = pairwise intros. 
NAMEGAME$GROUP <- factor(NAMEGAME$GROUP)
name.lm <- lm(RECALL ~ GROUP, data=NAMEGAME) # p = 3 treatments
summary(name.lm)
anova(name.lm)

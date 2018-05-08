setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/LYRICS.Rdata")
options(digits=10, show.signif.stars = FALSE)

lyrics.lm <- lm(SCORE ~ SONG*POOL, data=LYRICS)
summary(lyrics.lm)
anova(lyrics.lm)

# interaction test: not significant: p = 0.216 so the difference in mean
# agressivity for non/violent songs does not depend on pool type. 
# main: pool = not significant (mean aggressivity does not depend on pool type)
# main: song = signif (mean aggressivity does depend on song type)

anova(lm(SCORE ~ POOL*SONG, data=LYRICS))
# order of fit no matter. 
setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/BOXING.Rdata")


is.factor(BOXING$BOXER)
BOXING$BOXER <- factor(BOXING$BOXER)

boxing.lm <- lm(POWER ~ BOXER + ROUND, data=BOXING)
summary(boxing.lm)

# testing if difference in treatment means - yes there is, since p = 0.018
anova(boxing.lm)
# OR can do this way
boxer.lm <- lm(POWER ~ BOXER, data=BOXING)
NestedFTest(boxer.lm, boxing.lm)

# testing if difference in block means (is blocking effective?)
rounds.lm <- lm(POWER ~ ROUND, data=BOXING)
NestedFTest(rounds.lm, boxing.lm)
# OR
anova(rounds.lm, boxing.lm)

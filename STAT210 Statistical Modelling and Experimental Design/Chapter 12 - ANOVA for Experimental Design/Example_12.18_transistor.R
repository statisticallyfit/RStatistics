setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/TRANSISTOR1.Rdata")
options(digits=10, show.signif.stars = FALSE)


TRANSISTOR1$SHIFT <- factor(TRANSISTOR1$SHIFT)
TRANSISTOR1$DAYS <- factor(TRANSISTOR1$DAYS)
TRANSISTOR1$BREAKS <- factor(TRANSISTOR1$BREAKS)

transist.3way.lm <- lm(PRODUCT ~ SHIFT*DAYS*BREAKS, data=TRANSISTOR1)
summary(transist.3way.lm)
anova(transist.3way.lm)

# testing global
transist.null.lm <- lm(PRODUCT ~ 1, data=TRANSISTOR1)
anova(transist.null.lm, transist.3way.lm)
# OR
NestedFTest(transist.null.lm, transist.3way.lm)

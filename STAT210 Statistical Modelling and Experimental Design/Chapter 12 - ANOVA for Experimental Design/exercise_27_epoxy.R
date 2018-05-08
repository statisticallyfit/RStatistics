setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/EPOXY.Rdata")


is.factor(EPOXY$EXPTIME)
is.factor(EPOXY$SYSTEM)

EPOXY$EXPTIME <- factor(EPOXY$EXPTIME)
EPOXY$SYSTEM <- factor(EPOXY$SYSTEM)

subset(EPOXY, SYSTEM==1) #this case needs no fixing
EPOXY <- removeWhitespace(EPOXY)

epoxy.lm <- lm(CORRATE ~ EXPTIME + SYSTEM, data=EPOXY)
anova(epoxy.lm)

# there is a difference between treatment means (system) since
# the p-value in last row is 0.00036 which means we can reject H0 that
# B1 = B2 = B3 = 0 (treat means are equal) and conclude that they are different. 
setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/ACCHW.Rdata")

ACCHW$ASSIST <- factor(ACCHW$ASSIST)
subset(ACCHW, ASSIST == "NO")
# so we need whitespace function
ACCHW <- removeWhitespace(ACCHW, colsToFix="ASSIST")
subset(ACCHW, ASSIST == "NO")
is.factor(ACCHW$ASSIST)

account.lm <- lm(IMPROVE ~ ASSIST, data=ACCHW)
summary(account.lm)

anova(account.lm)

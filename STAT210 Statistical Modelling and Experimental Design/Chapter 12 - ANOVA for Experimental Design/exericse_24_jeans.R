setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/JEANS.Rdata")

JEANS$WEEK <- factor(JEANS$WEEK)

jeans.lm <- lm(ABSRATE ~ WEEK + DAY, data=JEANS )
summary(jeans.lm)
anova(jeans.lm)
# from anova: blocks = days, treatments = weeks so the treatment test is being
# done here because the 'DAY' line is testing the nested hypothesis test that
# the weeks parameters (reduced model) are 0. (block test => blocks params = 0, so 
# reduced model is the weeks params)
# F = 1.999, p = 0.118, so no mean difference in absentee rate over the day of
# the week. 
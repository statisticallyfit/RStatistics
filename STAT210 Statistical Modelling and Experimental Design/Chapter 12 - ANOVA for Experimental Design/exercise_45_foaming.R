setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/FOAM.Rdata")
options(digits=10, show.signif.stars = FALSE)


FOAM <- setNames(FOAM, nm=c("A", "C", "L", "F", "PCT_COPPER"))


foam.lm <- lm(PCT_COPPER ~ A + C + L + F + A:C + A:L + A:F + C:L + C:F + L:F, 
              data=FOAM)
summary(foam.lm)
anova(foam.lm)

# when all kways are fitted, the textbook formula of df_error applies
# df_error = abcd(r-1) and since r = 1, df_error = 0 so not enough
# df_error to estimate. 
foam.full.lm <- lm(PCT_COPPER ~ A*C*L*F, data=FOAM)
summary(foam.full.lm)

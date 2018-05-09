setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/NICKEL.Rdata")
options(digits=10, show.signif.stars = FALSE)

is.numeric(NICKEL$TIME)
NICKEL$TIME <- factor(NICKEL$TIME) # to get same result as in book. 

nickel.lm <- lm(YIELD ~ ALLOY*MATERIAL*TIME, data=NICKEL)
anova(nickel.lm)

# SIGNIFICANT interactions: 

# --- A:M:T: not signif. (ok to interpret the 2-way interactions)

# -- Alloy:Material: the difference in mean yield for the inconel and incoloy 
# levels of alloy depends on the material type (rolled, drawn). 
# -- Alloy: NO main effects test for alloy (since interac is signif).
# -- Material: NO main effects test for material. 

# -- ALLOY:TIME: the difference in mean yield for inconel and incoloy levels
# of alloy depends on the charging time level. 
# -- TIME - NO main effects test for time. 

# -- MATERIAL:TIME: not signif



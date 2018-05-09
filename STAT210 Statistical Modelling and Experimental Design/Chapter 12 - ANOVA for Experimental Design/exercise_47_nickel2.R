setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/NICKEL.Rdata")
options(digits=10, show.signif.stars = FALSE)

# now using numeric time
is.numeric(NICKEL$TIME)


nickel2.lm <- lm(YIELD ~ TIME + I(TIME^2) + ALLOY + MATERIAL + ALLOY:MATERIAL + 
                       TIME:ALLOY + TIME:MATERIAL + TIME:ALLOY:MATERIAL + 
                       I(TIME^2):ALLOY + I(TIME^2):MATERIAL + I(TIME^2):ALLOY:MATERIAL, 
                 data=NICKEL, x=TRUE)

summary(nickel2.lm)


nickelFactorsData <- NICKEL
nickelFactorsData$TIME <- factor(nickelFactorsData$TIME)

library(dae)
interaction.ABC.plot(response=YIELD, x.factor=TIME, groups.factor=MATERIAL,
                     trace.factor=ALLOY, data=nickelFactorsData)
interaction.ABC.plot(response=YIELD, x.factor=TIME, groups.factor=ALLOY,
                     trace.factor=MATERIAL, data=nickelFactorsData)


# exercise 48
reduced.lm <- lm(YIELD ~ TIME + I(TIME^2), data=NICKEL)
summary(reduced.lm)
anova(reduced.lm)

anova(reduced.lm, nickel2.lm)
df = NestedFTest(reduced.lm, nickel2.lm)
# so significant difference in the higher order terms, the higher order
# terms are necessary, choose complete model. 
df$RecommendedName

setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

library(ggplot2)
load("data/Exercises and Examples/PONDICE.Rdata")
options(digits=10, show.signif.stars = FALSE)


# NOTE: 1 = first year, 3 = landfast, 2 = multiyear
# must make 1 = landfast, 0 = multiyear
iceData <- PONDICE[PONDICE$icetype != 1, ]
iceData$icetype <- iceData$icetype-2


# part c)
ice.glm <- glm(icetype ~ depth + broadbandalb + visiblealb, data=iceData, family=binomial)
ice.null.glm <- glm(icetype ~ 1, data=iceData, family=binomial)
ice.next.glm <- glm(icetype ~ depth + broadbandalb, data=iceData, family=binomial)
summary(ice.glm)
anova(ice.glm)

# part d) overall model adequacy (residual deviance)
ResidualDevianceTest(ice.glm)
# book's model adequacy (test that all slopes are zero - deviance test)
DevianceTest(ice.glm)
# OR
anova(ice.glm, test="Chisq") # to test H0: B1 = B2 = B3 = 0, subtract the
# null model residual deviance with the last all-coefs model residual deviance
# = 70.45, df = 415 - 412 = 3



# part e) interaction pairwise model
ice.pair.glm <- glm(icetype ~ depth + broadbandalb + visiblealb + depth*broadbandalb + 
                          depth *visiblealb + broadbandalb*visiblealb, data=iceData,
                    family=binomial)
summary(ice.pair.glm)


# part g) comparing tested models
anova(ice.glm, ice.pair.glm, test="Chisq")
# deviance difference = 32.19, p-value = 0.000000476 so second model is
# highly better, reject H0: additional zero terms. 

# OR; 
NestedLikelihoodRatioTest(ice.glm, ice.pair.glm)

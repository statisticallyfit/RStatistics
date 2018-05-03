setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

library(ggplot2)
load("data/Exercises and Examples/PACGEESE.Rdata")
options(digits=10, show.signif.stars = FALSE)

geeseData <- data.frame(Response=as.numeric(PACGEESE$RESPONSE)-1, 
                        Altitude=PACGEESE$ALTITUDE, Lateral=PACGEESE$LATERAL)
geese.glm <- glm(Response ~ Altitude + Lateral, family=binomial, data=geeseData)

summary(geese.glm)

# global model fit
anova(geese.glm, test="Chisq")
# --- deviance = for the lateral-altitude model it is 252.47 with df = 1 and is 
# significant so the overall model is useful. (testing that all slopes are zero)
# --- resid deviance = for the lateral-altitude model it is 359.629 with df = 461
# and by below, it is not significant, so the deviation from expected is not large
# so the model is a good fit. 
ResidualDevianceTest(geese.glm) #




# part b) testing if altitude is useful for flight response (B1 = 0)
geese.noalt.glm <- glm(Response ~ Lateral, data=geeseData, family=binomial)
anova(geese.noalt.glm, geese.glm, test="Chisq")
# OR
LikelihoodRatioNestedTest(geese.noalt.glm, geese.glm)


# part c) testing if lateral is usefull for flight response (B2 = 0)
anova(geese.glm, test="Chisq")
# deviance = 252.475, p=2.2e-16
# OR
geese.nolat.glm <- glm(Response ~ Altitude, data=geeseData, family=binomial)
anova(geese.nolat.glm, geese.glm, test="Chisq")
# OR
LikelihoodRatioNestedTest(geese.nolat.glm, geese.glm)

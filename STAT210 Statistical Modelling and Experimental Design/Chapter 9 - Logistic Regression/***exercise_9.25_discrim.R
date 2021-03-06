setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

library(ggplot2)
load("data/Exercises and Examples/DISCRIM.Rdata")
options(digits=10, show.signif.stars = FALSE)


discr.glm <- glm(HIRE ~ EDUC + EXP + GENDER, data=DISCRIM, family=binomial)
discr.null.glm <- glm(HIRE ~ 1, data=DISCRIM, family=binomial)
summary(discr.glm)

# part a) # MODEL ADEQUACY (resid dev)
anova(discr.glm, test="Chisq") # we subtract the top and bottom resid devs
anova(discr.null.glm, discr.glm, test="Chisq") # result is the only one under deviance col
# with p-value = 0.00013
DevianceTest(discr.glm) #direct of above
ResidualDevianceTest(discr.glm) # model adequacy (called so by UNE)
# good fit model since p = 0.92 large so difference between expecteds
# and observed is small. 



# part b) H0: B3 = 0
anova(discr.glm, test="Chisq")


# part c) mean confidence interval
CI.link <- predict(discr.glm, newdata=data.frame(EDUC=4, EXP=0, GENDER=1), 
                   type="link",se.fit=T)[1:2]
logit.ci <- data.frame(fit=CI.link$fit, 
                       lwr=CI.link$fit - 1.96*CI.link$se.fit, 
                       upr=CI.link$fit + 1.96*CI.link$se.fit)
logit.ci
mean.ci <- exp(logit.ci)
mean.ci

# another way using direct response
CI.resp <- predict(discr.glm, newdata=data.frame(EDUC=4, EXP=0, GENDER=1), 
                   type="response",se.fit=T)[1:2]
mean.ci.direct <- data.frame(fit=CI.resp$fit, 
                        lwr=CI.resp$fit - 1.96*CI.resp$se.fit,
                        upr=CI.resp$fit + 1.96*CI.resp$se.fit)
mean.ci.direct

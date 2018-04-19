setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

library(ggplot2)
load("data/Exercises and Examples/ROADBIDS.Rdata")
options(digits=10, show.signif.stars = FALSE)


# status = 1 (fixed bid), status = 0 (competitive bid)
roads.glm <- glm(STATUS ~ NUMBIDS + DOTEST, family="binomial", data=ROADBIDS)

# NOTE: the p-values of the coeffs are from this: each (Bi/sbi)^2 has
# a chi square distribution with df = 1. So p-value is calculating
# pchisq(q=zvalue, df=1, lower.tail=FALSE)
summary(roads.glm)
anova(roads.glm, test="Chisq")

# resid deviance
roads.glm$deviance
roads.glm$df.residual
# null deviance
roads.glm$null.deviance
roads.glm$df.null


nullMod <- glm(STATUS ~ 1, family="binomial", data=ROADBIDS)

res <- LikelihoodRatioGLMCompareTest(nullMod, roads.glm)
res

res <- LikelihoodRatioGLMTest(roads.glm)
res





# PREDICTIONS -- help how to do prediction plotting? 
attach(ROADBIDS)
preds <- predict(roads.glm, newdata=data.frame(NUMBIDS), type="link", se.fit=TRUE)
crit <- 1.96
# real fits, uprs, lwrs
predData <- data.frame(fit.real=roads.glm$family$linkinv(preds$fit),
                       upr.real= roads.glm$family$linkinv(preds$fit + crit * preds$se.fit),
                       lwr.real = roads.glm$family$linkinv(preds$fit - crit * preds$se.fit))
predData$NUMBIDS <- seq(min(NUMBIDS), max(NUMBIDS), length=nrow(predData))
predData$fit.logodds <- preds$fit

head(predData)


p.data = ggplot(ROADBIDS, aes(x=NUMBIDS, y=STATUS)) + 
      geom_point(shape=19, size=3) 

p.fits = p.data + 
      stat_smooth(method="glm", method.args=list(family=binomial))
      #geom_line(data=predData, aes(y=fit.real, col="blue"),size=1) +
      #geom_line(data=predData, aes(y=lwr.real, col="red"),size=1) + 
      #geom_line(data=predData, aes(y=upr.real, col="red"),size=1) 
p.fits






# TESTING: Do any of the higher order terms improve the model? 
roads.higher.glm <- glm(STATUS ~ NUMBIDS*DOTEST + 
                              I(NUMBIDS^2) + I(DOTEST^2), family="binomial",
                        data=ROADBIDS)
summary(roads.higher.glm)
anova(roads.higher.glm)

# Test hypothesis: β3  = β4  = β5  = 0
# chi_compare = chi_complete - chi-reduced and
# df_compare = df_complete - df_reduced 
# OR
# chi_compare = chi_reduced - chi_complete and
# df_compare = df_reduced - df_complete

LikelihoodRatioGLMTest(roads.glm)
LikelihoodRatioGLMTest(roads.higher.glm)
LikelihoodRatioNestedGLMTest(roads.glm, roads.higher.glm)

#should equal the likelihood nested glm test
anova(roads.glm, roads.higher.glm, test="Chisq")

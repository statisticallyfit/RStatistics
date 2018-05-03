setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

library(ggplot2)
load("data/Exercises and Examples/ROADBIDS.Rdata")
options(digits=10, show.signif.stars = FALSE)


# status = 1 (fixed bid), status = 0 (competitive bid)
roads.glm <- glm(STATUS ~ NUMBIDS + DOTEST, family="binomial", data=ROADBIDS)
# sample is broken down in individual bids, so no WEIGHTS of sample needed. 


roads.numbids.glm <- glm(STATUS ~ NUMBIDS, data=ROADBIDS, family=binomial)

# NOTE: the p-values of the coeffs are from this: each (Bi/sbi)^2 has
# a chi square distribution with df = 1. So p-value is calculating
# pchisq(q=zvalue, df=1, lower.tail=FALSE)
cof <- summary(roads.glm)$coef
exp(cof) # odds ratios



# TESTING MODEL DEVIANCES
anova(roads.glm, test="Chisq")
# So deviances and their p-value sin the column test whether the predictors are 0. 
# second row: null model vs. numbids model
DevianceTest(roads.numbids.glm) #  significant diff between them
# third row: numbids model vs. numbids + dotest model
LikelihoodRatioNestedTest(roads.numbids.glm,roads.glm) # signif diff between them

# not in anova here: null model vs. numbids + dotest model
DevianceTest(roads.glm) # nested between null and roads


# TESTING MODEL RESIDUAL DEVIANCE
anova(roads.glm, test="Chisq") # p = 0.74 (below) so not sig so good model.
ResidualDevianceTest(roads.numbids.glm) # good model numbids since resid dev is small 
ResidualDevianceTest(roads.glm) # global fit of numbids + dotest model



# resid deviance
roads.glm$deviance
roads.glm$df.residual
# null deviance
roads.glm$null.deviance
roads.glm$df.null




# PLOTTING CONFIDENCE INTERVALS
plotConfidenceBands.glm(roads.numbids.glm)



# TESTING: Do any of the higher order terms improve the model? 
roads.higher.glm <- glm(STATUS ~ NUMBIDS*DOTEST + 
                              I(NUMBIDS^2) + I(DOTEST^2), family="binomial",
                        data=ROADBIDS)
summary(roads.higher.glm)
anova(roads.higher.glm, test="Chisq")

# Test hypothesis: β3  = β4  = β5  = 0
# chi_compare = chi_complete - chi-reduced and
# df_compare = df_complete - df_reduced 
# OR
# chi_compare = chi_reduced - chi_complete and
# df_compare = df_reduced - df_complete

# doing overall global fit test for roads glm
ResidualDevianceTest(roads.glm)
anova(roads.glm) # tests not in this table at all

# comparing between null model and roads higher model
DevianceTest(roads.higher.glm)
anova(roads.higher.glm, test="Chisq") # anove only compares sequential models, not
# jumping between levels of term-adding. 

LikelihoodRatioNestedTest(roads.glm, roads.higher.glm)
#should equal the likelihood nested glm test
anova(roads.glm, roads.higher.glm, test="Chisq")





# Lecture pred numbids = 5, dotest=-10
predict(roads.glm, type="response", newdata=data.frame(NUMBIDS=5, DOTEST=-10))
# so estimated probability of a fixed bid when you have 5 bids and dotest = -10
# is about 0.03

# interpret coefs
cof <- summary(roads.glm)$coef
exp(cof[,1]) -1
# --- So for each additional increase in NUMBIDS, the odds of a fixed contract
# will decrese by 53%, holding DOTEST fixed. 
# --- For each additional increase in DOTEST, the odds of a fixed contract will
# increase by 11.87%, holding NUMBIDS fixed. 
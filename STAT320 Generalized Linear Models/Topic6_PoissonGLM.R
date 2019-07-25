setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(show.signif.stars = FALSE)
options(scipen=999) # disable scientific notation
options(digits=10)

## Example 6.1 - Faults Data, model with Poisson glm --------------------------------------

fabricData <- read.table("data/fault.txt", header=TRUE)

# Response: number of faults, which occur at random at a fixed rate per meter
# (use poisson count distribution)

# Plotting the length of a roll vs number of faults
ggplot(data=fabricData, aes(x=faults)) + 
     geom_density(size=1, alpha=0.3, fill="pink")

# Scatterp;ot of faults vs length of roll
ggplot(data=fabricData, aes(x=length, y=faults)) + geom_point(colour="dodgerblue")



## Fitting the model
fault.glm <- glm(faults ~ length, family=poisson, data=fabricData)
null.glm <- glm(faults ~ 1, family=poisson, data=fabricData)


#### DEVIANCE CALCULATION  ###############################################

# Same things: nested likelihood ratio / anova / deviance tests are the same:
anova(fault.glm, test="Chisq")
NestedLikelihoodRatioTest(null.glm, fault.glm)
DevianceTest(fault.glm) # same as the anova of fault.glm, above

# is same as ...
null.glm$deviance - fault.glm$deviance
##########################################################################


#### RESIDUAL DEVIANCE CALCULATION #######################################
ResidualDevianceTest(fault.glm)
anova(null.glm, fault.glm, test="Chisq")
# is same as ...
fault.glm$deviance
##########################################################################

#### DF CALCULATION ######################################################
n <- nrow(fabricData); n
k <- length(fault.glm$coefficients) - 1; k 

df.null <- n - 1; df.null
df.residual <- n - k - 1; df.residual 
##########################################################################


# Summary of the model
summary(fault.glm)

# Sign of overdispersion: the residual deviance 61.75 is much larger than
# residual df, 30. 

# OVERDISPERSION: inflated residual deviance (when residual deviance G >> residual df)
# If the Poisson model fits the data reasonably, we would expect the
# residual deviance to be roughly equal to the residual degrees of freedom.
# That the residual deviance is so large suggests that the conditional variance 
# of the expected number of faults exceeds the variance  of a 
# Poisson-distributed variable, for which the variance equals the mean. 
# This common occurrence in the analysis of count data is termed overdispersion.

# ISSUES: if you let overdispersion occur, your coefficient estimates will be ok
# but the inference (CI's, p-values) will not be reliably. 

# REASONS: 
# 1. research did not include all the necessary predictors
# 2. the response Y may not be poisson, where E(Y) = Var(Y)
# 3. the observations are not independent. 



# # --------------------------------------------------------------------------------
# METHOD 1 to deal with overdispersion: Negative binomial model
# If Y ~ NegativeBinomially distributed, then Var(Y) = phi * E(Y), where
# phi is a constant, called "dispersion" parameter, and phi > 1 (can be estimated from data)
 #When phi > 1, then conditional variation of Y, var(Y_i|n_i), increases more
# rapdily than its mean, mu_i. 




# ----------------------------------------------------------------------------------
# METHOD 2 to deal with overdispersion: quasi-likelihood model 
# BENEFIT: if overdispersion occurred from wrongly assuming Y is poisson, then
# using a quasi-poisson can deal with overdispersion in the best way. 

# Quasipoisson alows greater variability in the data than would be expected
# from a simple poisson. Allows clustering around some Y values. 

# CHANGES TO THE QUASI MODEL: let phi-bar be fit from the data. Then the standard errors
# of the coefficients in the quasi-poisson model are sqrt(phi-bar) times those for 
# the original poisson model. Thus the quasi poisson stderrors are larger => F-tests
# for the terms in the model give smaller F-statistics => larger p-values. 

# FORMULA FOR DISPERSION PARAMETER, estimated from data: 
# phi-bar ~ residualdeviance / (n - k - 1)
res = ResidualDevianceTest(fault.glm)

dispersionEstimate <- res$LikRatio / res$df # (approximate)
dispersionEstimate


# Fitting the quasi poisson model: GLM with quasi poisson distribution
fault.quasi.glm <- glm(faults ~ length, family=quasipoisson, data=fabricData)
anova(fault.quasi.glm, test="Chisq")
# Deviance (difference in residual deviances) = 41.95
# Residual deviance of this model = 61.758
anova(fault.glm, test="Chisq")
# same as the old model

# Only the standard errors are different, coeffs are the same: 
summary(fault.quasi.glm)

stdErrs.df <- data.frame(cbind(OriginalStdErr=summary(fault.glm)$coef[,2], 
      QuasiStdErr=summary(fault.quasi.glm)$coef[,2]))
stdErrs.df

# Relation between old and new standard errors: 
stdErrs.df$OriginalStdErr[1] * sqrt(dispersionEstimate)
stdErrs.df$OriginalStdErr[2] * sqrt(dispersionEstimate)




### DEVIANCE RESIDUALS 
DevianceResiduals.Poisson(fault.glm)
# is same as
residuals(fault.glm)
fault.glm$residuals # pearson residuals, not same as deviance residuals

# Plotting the deviance residuals: the discrepancy between observed and predicted
# values. They are approximately normal for a well-fitting modl. 
residualFitPlot(fault.glm)

# All the diagnostic plots:
autoplot(fault.glm, which=c(1,2,3,4))
# Deviance residuals: are approximately normally distributed, as they should be
# for a wel--fitting model. 
# No pattern in residuals vs fits plot, so no heteroskedasticity
# Thus: di ~ N(0, sigma^2) is satisfied. 

# Influence measures
cooks.df <- influence.cooksDistances(fault.glm)
cooks.df
cooks.df[13,]
cooks.distance(fault.glm)






# SECTION 6.3: Contingency Table (2x2) -------------------------------------------
# P = present, A = absent, GallA = wasp gall species A, GallB = species B
gallData.long <- data.frame(GallA=c(rep("P", 57), rep("A", 54)),
                       GallB=c(rep("P",13), rep("A",44), rep("P",25), rep("A",29)))
tbl <- table(gallData.long)
mtbl <- marginalTable(tbl); mtbl

gallData <- data.frame(count=c(13, 44, 25, 29), gallA=c(rep("P", 2), rep("A", 2)), gallB=c("P", "A", "P", "A") )

# Chisquare Test of Independence ==================================================
X2 <- chisq.test(tbl, correct=F); X2
# p-value = 0.009 so there is a big difference between observed counts and expected
# counts. So the species A and B of gall are NOT independent.

X2$observed
X2$expected
# From the observed vs expected values that the expected values are often
# smaller than observed. So the two species of gall occur TOGETHER ON SAME TREE
# less frequenty than EXPECTED. 


# POISSON GLM instead of chisquare ================================================
# The model is: 
# Y_ij ~ Poisson(mu_ij)
# log(u_ij) = mu + row_i + col_j

# MODEL OF INDEPENDENCE OF ROW and COL: no interaction term. (Y ~ row + col)
gall.indep.glm <- glm(count ~ gallA + gallB, data=gallData, family=poisson)
# put here saturated model as well
gall.saturated.glm <- glm(count ~ gallA * gallB, data=gallData, family=poisson, x=T)

#anova(gall.indep.glm, test="Chisq") # note: the p-values correspond to the DEVIANCEs
# for each row and column (each term in the glm), NOT to the residual deviance.

ResidualDevianceTest(gall.indep.glm, printNice=F) # reject H0, model not good fit
# Nested Test: testing H0: model of independence, Ha: complete model. This just
# effectively does a global F-test for the interaction terms. If the p-value is < 0.05
# then can reject H0. Here, p-value for deviance is 0.008724 so can reject H0. 
# The hypothesis of independence does not stand. The independence model is
# not adequate. 
# Short conclusion: p-value = 0.008 < 0.05 ===> reject h0 ===> model of independence
# does not hold ===> the interaction terms are nonzero ===> there is an association
# between species A, B of gall wasps. 
NestedLikelihoodRatioTest(gall.indep.glm, gall.saturated.glm, printNice=T)
# same thing as: 
anova(gall.indep.glm, gall.saturated.glm, test="Chisq")


# Deviance Test compares the null model with the given model
#DevianceTest(gall.indep.glm, printNice=F) # reject H0, model not statistically useful
# same as: #nested likelihood of gall.indep with null model 


# misc: could also comparing the A and A + B models
ga.glm <-glm(count ~ gallA, data=gallData, family=poisson)
anova(ga.glm, gall.indep.glm, test="Chisq")
NestedLikelihoodRatioTest(ga.glm, gall.indep.glm, printNice=F)


# SATURATED MODEL: with interaction term (dependence model)
gall.saturated.glm <- glm(count ~ gallA * gallB, data=gallData, family=poisson, x=T)

anova(gall.saturated.glm, test="Chisq") # p-values are for deviance not resid deviance
# INTERPRET: the last row tests the interaction term, just like the nested test:


# note: expected values in chisquare === predicted values in glm
predict(gall.indep.glm, type="response")
X2$expected

# Confirming prediction for: gallA = Present, gallB=absent
cs <- coef(gall.indep.glm); cs
# then we have gallA=1, gallB=0
exp(cs[1] + cs[2]*1 + cs[3]*0)
# same as row = 2, col=1 in the expected table: 
X2$expected 



## THREE WAY TABLE (UNPOOLED + CORRECT) -------------------------------------------

# RESEARCH QUESTION: does attack by aphids lead to a reduction in proportion of
# leaves holed? (association between aphids infestation and presence/absence of holes).
# Statisticall speaking: is the aphid:hole interaction term significant? 
aphidData <- read.table("data/aphiddefence.txt", header=TRUE)
aphidData$tree <- factor(aphidData$tree)

# Fit a glm with no pooling

# STEP 1: fit the SATURATED MODEL: always fit the saturated model to start with.
aphid.saturated.glm <- glm(count ~ tree*aphid*hole, data=aphidData, family=poisson)
anova(aphid.saturated.glm, test="Chisq")
# INTERPRET: deviance p-value = 0.977 for three-way interaction term ==>
# no evidence of three way interaction between tree, aphid, and hole. 

# STEP 2: remove the three-way term, just keep all two-way terms. 
aphid.alltwoway.glm <- glm(count ~ (tree + aphid + hole)^2, data=aphidData, family=poisson)
anova(aphid.alltwoway.glm, test="Chisq")
# There is evidence of tree:aphid, and tree:hole interaction, but since p-value is
# 0.954 for deviance of aphid:hole interaction term, we can remove that. 

# Question: ???
# anova tests terms consecutively, better
# to use the summary table to test significance of all terms at once to determine
# which are better to remove, given all others have been fitted. ?????????????
# summary(aphid.alltwoway.glm)


# STEP 3: removing unnecessary interactions: aphid: hole
aphid.twoway.glm <- update(aphid.alltwoway.glm, ~. -aphid:hole, data=aphidData, family=poisson)
anova(aphid.twoway.glm, test="Chisq")
# INTERPRET: all terms are important now


# STEP 4: comparing all the models
anova(aphid.twoway.glm, aphid.alltwoway.glm, aphid.saturated.glm, test="Chisq")
NestedLikelihoodRatioTest(aphid.twoway.glm, aphid.alltwoway.glm, printNice=F)
NestedLikelihoodRatioTest(aphid.alltwoway.glm, aphid.saturated.glm, printNice=F)

# When p-value is high, can say that the missing term ebtween the two models
# being compared is not significant => no association for those terms. 

# INTERPRET: 
# -> p-value = 0.9777 of deviance: means tree:aphid:hole interaction term was not
# significant => no evidence of association between tree,aphid,hole. 
# -> p-value = 0.954 of deviance 0.00329: means aphi:hole interaction was not
# significant => no evidence of non-independence between aphid and hole.

# ----> ANSWER TO RESEARCH QUESTION: no dependence between aphids and hole. 

# NOTE: the order of fitting is important: if we fit the aphid,hole terms first
# and the tree term last, then the aphid:hole interaction term is equivalent
# to testing for an interaction pooling the data for the two trees. 

# CHECKING: 
# (1) WRONG WAY 1: FITTING THE TREE TERM LAST (wrong way)
aphid.saturated.treelast.glm <- glm(count ~ aphid*hole*tree, data=aphidData, family=poisson)
anova(aphid.saturated.treelast.glm, test="Chisq")
# remove the last three-way term
aphid.alltwoway.treelast.glm <- glm(count ~ (aphid + hole + tree)^2, 
                                    data=aphidData, family=poisson)
anova(aphid.alltwoway.treelast.glm, test='Chisq')
# INTERPRET: aphid:hole interaction p-value = 0.009 for deviance


# (2) WRONG WAY 2: POOLING THE DATA: 
pooledcounts <- c(181, 3392, 53, 1479)
pooledaphid <- c(rep("no",2), rep("yes",2))
pooledhole <- c("yes", "no", "yes", "no")
aphidPooledData <- cbind.data.frame(pooledcounts, pooledaphid, pooledhole)
aphidPooledData

aphid.pooledtree.glm <- glm(pooledcounts ~ pooledaphid + pooledhole, 
                            data=aphidPooledData, family=poisson)
anova(aphid.pooledtree.glm, test="Chisq")

ResidualDevianceTest(aphid.pooledtree.glm, printNice=F)

# INTERPRET: residualdeviance G = 6.7 with p-value = 0.009 => significant ==> 
#suggests that attack by aphids does have an effect on holes. 
# (must fit hole term conditional on aphid term)
setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
options(show.signif.stars = FALSE)


 #Need to store data differently for binomial compared to proportion

aphidData.poisson <- read.table("data/aphiddefence.txt", header=TRUE)
aphidData.poisson$tree <- factor(aphidData$tree)
aphidData.poisson

aphidData.binomial <- data.frame(tree=factor(c(1,1,2,2)), 
                                 aphid=factor(rep(c("no","yes"), 2)), 
                                 hole=c(35, 23, 146, 30),
                                 total=c(1785, 1169, 1788, 363))
aphidData.binomial

# response = proportion of leaves with holes in them

# Fit the independence model: 
aphid.indepbinom.glm <- glm(hole/total ~ tree + aphid, data=aphidData.binomial, family=binomial, weights=total)
# Fit the saturated model: 
aphid.saturatedbinom.glm <- glm(hole/total ~ tree * aphid, data=aphidData.binomial, family=binomial, weights=total)

anova(aphid.saturatedbinom.glm, test='Chisq')
anova(aphid.indepbinom.glm, test='Chisq')
# NOTE: the residual deviance of the independence model is the same as the deviance
# for the tree:aphid interaction term in the saturated model

anova(aphid.indepbinom.glm, aphid.saturatedbinom.glm, test="Chisq")



# INTERPRET: 
anova(aphid.saturatedbinom.glm, test='Chisq')

# Deviance of independence model = 0.00329, p-value = 0.954
# ==> so the aphid predictor is not sigificant given that the tree predictor was fitted. 

# Residual Deviance of independence model = 0.000791, p-value = 0.97756
# ===> independence model is a good fit.
1 -pchisq(0.000791, df=1)

# Deviance of saturated model = 0.000791, p-value = 0.97756
# ===> so the interaction term tree:aphid is not significant. The inedpenednece model
# seems to be the best fit. 
NestedLikelihoodRatioTest(aphid.indepbinom.glm, aphid.saturatedbinom.glm)

# Residual deviance of saturated model ~ 0, p-value = 1 
# ===> so no evidence to reject null that model fits. The saturated model captures
# the data variation perfectly (overfits).
1 - pchisq(0, df=0)


# INTERPRET: second line in independence model: 
anova(aphid.indepbinom.glm, test="Chisq")
# aphid term: aphid infestation after adjusting for differences between trees is not
# significant (p = 0.954, with deviance = 0.00329)
# ===> so conclude the likelihood of a leaf being holed is the same regardless of earlier
# infestation by aphids. (same conclusion as per poisson model)

# ANSWER TO RESEARCH QUESTION: probability of leaf beind holed is the same regardless of
# earlier infestation by aphids. Means: no interaction between aphids and holes. 
# In poisson model, the aphid:hole term is not significant: 
anova(aphid.poissontwoway.glm, aphid.poissonalltwoway.glm, test="Chisq")



# EQUIVALENCE BETWEEN BINOMIAL AND POISSON MODELS EXPLAINED:  ============================

### Binomial: hole/total ~ tree + aphid
# Test question: use anova table - is aphid term significant?

### Poisson: count ~ tree + aphid + hole + tree:aphid + tree:hole + aphid:hole
# Test question: use anova table - is aphid:hole term significant?

# the p-values of the tree:hole, aphid:hole in poisson correspond to p-values of the
# tree, aphid terms in the binomial. 
# =======================================================================================
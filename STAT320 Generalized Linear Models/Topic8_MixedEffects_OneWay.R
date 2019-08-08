setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#detach(package:lme4)
#detach(package:nlme)
library(nlme)
options(show.signif.stars = FALSE)


### Example 8.1: --------------------------------------------------------------

# ONE FIXED EFFECT ------------------------------------------------------------

sireData <- read.table("data/calfwt.txt", header=TRUE)
head(sireData)

# Plot the data with sire as a factor and weight as the numerical variable
sireData$sire <- factor(sireData$sire)

ggplot(data=sireData, aes(x=sire, y=weight, color=sire)) + geom_boxplot(size=1)
# INTERPRET: 
# Variation amongst mean weights, shown by different median value line in 
# the boxes. 
# Variance of weights differs from level to level of sire (shown by different
# box widths per sire level 1,2,3,4,5,6)

# Fit the linear model with one fixed effect
sire.lm <- lm(weight ~ sire, data=sireData)
anova(sire.lm)
# INTERPRET: sire p-value of F-statistic is 0.01 so sire is a significant
# predictor of weight. This model is statistically useful. 

# Fit the no-intercept model
sire.nointercept.lm <- lm(weight ~ sire - 1, data=sireData)
anova(sire.nointercept.lm) # not the same

# Compare coefficients
summary(sire.lm) # coefficients are difference of means between current
# sire level (2,3,4...) and base level 1
summary(sire.nointercept.lm) # coefficients are exactly the mean weights
# and not differences any longer, since intecept is eliminated. 


# Confidence intervals
ci.nointercept <- betaCI(sire.nointercept.lm)
ci.nointercept
## sire 2: we are 95% confident that the population mean weight for sire2
# is between 71.207 and 113.29
## Sire1 made calves with significantly lower birth weight than sires 3,5,6
# (since its confint is all lower tha the CI's of sires 3,5,6). But sire1's calves
# did not have significantly lower birth weight than sires 2,4 since their conf ints
# are overlapping. 
# Between 3,5,6: insufficient evidence to distinguish between birth weights
# of calves from sires 3,5,6. 


## MEANING OF FIXED EFFECT: ====================================================
# if we are interested in the performance of those six bulls 
# in particular (specifically those fixed bulls, just them), ONLY THEN can
# sire be regarded as a FIXED effect. Then we are interested to estimate
# the mean weight of calves sired by each bull: mu_1, mu_2, ..., mu_6. 
# ==============================================================================


### ONE RANDOM EFFECT ----------------------------------------------------------

## MEANING OF RANDOM EFFECT: ===================================================

#If sires are regarded as a RANDOM SAMPLE from a population of all sires available
# in that breed, then we are no longer interested in the performance of a 
# particular bull but of the performance in general of bulls that breed. 
# We are looking to evaluate the POPULATION from where our sample came, instead of
# a fixed set of bulls. 

# SO: sire can be regarded as a RANDOM EFFECT. 
# Then we want to estimate the variance components: 

# (1) BETWEEN variation: sigma_sire ^ 2, which is  variability in calf weight 
# "between" sires. 
# (2) WITHIN variation: sigma^2 = residuals variance, which is variability
# in calf weight "within" sires. So for instance, for sire1, this is sigma^2. 

# ==============================================================================


# Fit the linear mixed model with one random effect
# Fixed part: the overall mean is fixed
# Random part: sire is random. The random effects model is  ~1|sire which means
# "the intercepts, given the factor levels of sire". 
sire.randomeffect.lme <- lme(weight ~ 1, random = ~1|sire, data=sireData)
sire.randomeffect.lme

summary(sire.randomeffect.lme)

# NOTE: 
# The variance components for the random effects are expressed as standard deviations
# not variances. 
ci.random <- intervals(sire.randomeffect.lme)
ci.random

# INTERPRET: 

# random effect CI stdev: (7.954, 40.81)
# random effect: sigma_sire_hat = 18.017

# fixed effect CI stdev: (14.449, 27.77135)
# fixed effect est: sigma_hat = 20.03


# To get variances, must square the standard deviations: 
var.sire <- 18.017^2 # between group (sire) variance
var.res <- 20.0326^2 # within group (sire) variance

# OR: use VarCorr to get these variance components: 
VarCorr(sire.randomeffect.lme)

# VARIANCE OF AN OBSERVATION: 
# Var(Y_ij) = sigma_A^2 + sigma^2 = betweenGrouPVariance + withinGrouPVariance
# since the variances come from independent random variables. 

# So, estimated variance in weight of ONE CALF (one observation Y) is: 
var.sire + var.res


# REPEATABILITY: 
# Is the proportion of variance in weight that is due to the factor A
# repeatability = sigma_A_hat^2 / (sigma_A_hat^2 + sigma_hat^2)
repeatability.sire <- var.sire / (var.sire + var.res)
repeatability.sire
# INTERPRET: 
# About 45% of the variability in calf weight is due to the difference in sires. 

summary(sire.randomeffect.lme)
# See the Fixed Effects portion of output, under: weight ~ 1
# INTERPRET: 
### Intercepts row: The fixed effect is the overall mean = 104.79, and
# its standard error = 8.42. 
### t-test: tests hypothesis that the fixed effect is zero. p-value <0.0001
# indicates overall mean is significantly different from zero. 
### Five number summary of residuals



# Another way to make the model: using lmer
library(lme4)

# NLME way: 
# sire.lme <- lme(weight ~ 1, random = ~1|sire, data=data)

#LME4 way: 
sire.lmer <- lmer(weight ~ 1 + (1|sire), data=sireData)
sire.lmer
summary(sire.lmer)
# Better: gives the within group and between group variances right away!
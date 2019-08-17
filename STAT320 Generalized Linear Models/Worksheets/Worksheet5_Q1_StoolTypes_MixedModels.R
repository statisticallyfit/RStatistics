setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#detach(package:lme4)
#detach(package:nlme)
library(nlme)
#library(lme4)
library(lattice)
options(show.signif.stars = FALSE)

# Data: measures of effort required by 9 subjects to arise from each of 
# the 4 stool types
# Design: randomized block with each subject being a block. 

# RANDOM EFFECT: subject (since it is a random sample for a population)
# FIXED EFFECT: stool (since we investigate it at fixed levels, not random sampled)

data("ergoStool")

head(ergoStool)
ergoStool$Type
ergoStool$Subject

stool.lme <- lme(effort ~ Type, random = ~1|Subject, data=ergoStool)
stool.lmer <- lmer(effort ~ Type + (1|Subject), data=ergoStool)
anova(stool.lme)
anova(stool.lmer)

# coefs and p-values for fixed effects
s1 = summary(stool.lme)
s1$tTable 

# Get the variance components
VarCorr(stool.lme)

# Or get full analysis by lmer
summary(stool.lmer)


# Estimates of random intercepts for Random Effect (subject)
cofs <- random.effects(stool.lme)
cofs # the no-interaction  model

# Check model assumptions
plot(stool.lme$fitted[,1], stool.lme$residuals[,2])

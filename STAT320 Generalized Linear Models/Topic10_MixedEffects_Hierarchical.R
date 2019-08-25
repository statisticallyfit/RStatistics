setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
library(lme4)
library(lattice)

options(show.signif.stars = FALSE)


### HIERARCHICAL MODEL DATA: 

## For a hierarhicical (multilevel) model, the data must be structured in groups
# and regression coefficients can vary by groups. 

## Any mixed model (containing at least one random effect) is also a hierarhiccal model

## New idea here: grouped data. Each  plant (random effect) consists of a group of 
# repeated measures. 

#There are two levels of variability:
# (1) measurement error on each individual within a group. 
# (2) variability amongst groups. 

# Hieriarchical model: 
# - there is a regression line for each random effect
# - each parameter in the regression line of each random effect is a random sample
# from a populatio of parameters which are normally distribued as: 
#     B_j_i ~ N(B_j,  sigma_j^2)
# where j = the id on the predictor coefficient (j = 1 .. num predicors)
#       i = the id of the regression line (i = 1 ... number of levels in the random effect)


# CNETERING A PREDICTOR VARIABLE: 

# WHEN TO CENTER: 
## 1. to lessen correlation between a multiplicative term (like an interaction 
# or polynomial term and its component variables that make up this multiplicative term). 
## 2. to make parameters easier to interpret (we can center the predictor variable
# around a particular value if that value is meaningful)

# WHEN TO NOT CENTER: 
## 1. if all continuous predictors have a meaningful zero value. 
## 2. Do not center predictor X_j_i if there is no interaction term invovling X_j_i
## 3. Do not center predictor X_j_i if there are no other values of X_j_i that are
# meaningful. 

plantData <- read.table("data/fertilizer.txt", header=TRUE)
head(plantData)
plantData$plant   
sum(plantData$plant == "ID1")
sum(plantData$plant == "ID2")

plantData$fertilizer


plantControlData <- subset(plantData, subset = fertilizer == "control")
plantAddData <- subset(plantData, subset = fertilizer == "added")

# Exploratory plots
# formula = response ~ primaryCovariate | groupingFactor
# Can write number (1) instead of primaryCovariate when no other suitable candidate.
groupedPlantControlData <- groupedData(root ~ week | plant, data=plantControlData)
groupedPlantControlData

groupedPlantAddData <- groupedData(root ~ week | plant, data=plantAddData)

plot(groupedPlantControlData)
#plot(groupedPlantAddData)
# INTERPRET: for fertilizer = control, 
# profiles of each plant type over the weeks 2 - 10, and the root length
# response. As weeks increase, root length grows similarly for all plants but
# intercepts may be different. 

control.xyp <- xyplot(root ~ week, data=groupedPlantControlData, groups=plant, 
                    panel=panel.superpose, panel.groups=panel.lmline)
control.xyp
#added.xyp <- xyplot(root ~ week, data=groupedPlantAddData, groups=plant, 
#                    panel=panel.superpose, panel.groups=panel.lmline)
#added.xyp
# INTERPRET: Shows the root lengths over 5 fortnights for 6  randomly chosen plants
# and the superposed linear fits for each plant. 
# SUGGESTS: similar slopes (rate of growth per plant) but intercepts are different.

## NOTE: there are only 5 samples per plant, so we are restricted to fitting 
# linear regressions of root length as a function of week. 
# (not enough degrees freedom for interaction terms?)




### FITTING RANDOM INTERCEPTS + RANDOM SLOPES -------------------------------------

# Center the variable week. 
groupedPlantControlData$week.centred <- groupedPlantControlData$week - mean(groupedPlantControlData$week)
plantControlData$week.centred <- plantControlData$week - mean(plantControlData$week)
head(groupedPlantControlData)

# Fit the random effects model
# Y_ij | b_i ~ N(mu_ij | b_i, sigma^2)
# Means: response root conditional on plant (i)
# RANDOM EFFECT: plant
# FIXED EFFECT: week.centred
plant.lme <- lme(root ~ week.centred, data=plantControlData, random = ~ week | plant)
## ???plant.lmer <- lmer(root ~ week.centred + (week|plant), data=plantControlData )
summary(plant.lme)

VarCorr(plant.lme)

# TODO: why doesn't the week.centred variable show up ? in the varcorr like in the
# lecture notes page 123?

# Results: 
# sigma_intercept^2 = 0.11
# sigma_1^2 = 0
# sigma_residual^2 = 0.92

# Since variance component of slope (sigma_1) is small, we can see there is no difference
# among the plants in the rate of gorwth of roots, so there are equal slopes.
# So can fit the random intercepts only model. 




### RANDOM INTERCEPTS (only) -----------------------------------------------------

# Y_ij | b_i ~ N(mu_ij | B_i, sigma^2)
# mu_ij | b_i = Beta_0_i + Beta_1 (x_j - x-bar)
# Beta_0_i ~ Normal(Beta_0, sigma_0^2)

# Fit the random intercepts only model
plant.intercept.lme <- lme(root ~ week.centred, data=plantControlData, random = ~ 1 |plant)
summary(plant.intercept.lme)
VarCorr(plant.intercept.lme)

# Testing that the random intercepts model is preferred. 
anova(plant.intercept.lme, plant.lme)
# AIC of the intercepts only model is lower so it is preferred. 
# p-value is high so the extra random slope term in plant.lme is not useful. 
# However difference delta = 4 in AIC's is not great. 

# Random slope / random intercepts model can also be written: 

# Y_ij | b_i ~ Normal(mu_ij | B_i, sigma^2)
# mu_ij | b_i = (Beta_0 + b_0_i) + (Beta_1 + b_1_i)(x_j - x-bar)
# b_0_i ~ Normal(0, sigma_0 ^2)
# b_1_i ~ Normal(0, sigma_1^2)

# where the random components b's are the EFFECTS of each plant (level) on the
# intercept and slope. 

# Extract using ranef()
ranef(plant.lme)

# Can see no significant differences amongst slopes (coefficients of week)

## TODO: the second column is week not WEEK.CENTRED .. (???)
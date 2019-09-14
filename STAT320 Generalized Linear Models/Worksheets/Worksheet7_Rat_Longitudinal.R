setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
#library(lme4)
#detach(package:lme4)
library(lattice)

options(show.signif.stars = FALSE)


# CONCEPTS REVIEW: ---------------------------------------------------------------------

# SAME IDEA AS IN TOPIC 10 (HIERARCHICAL MODELS): 
# Idea: grouped data. Each  random effect consists of a group of 
# repeated measures. 
# There are two levels of variability:
# (1) measurement error on each individual within a group. 
# (2) variability amongst groups


# Data:  --------------------------------------------------------------------------------
# Replications = 5 (5 weekly measures on each rat)
# N = 27 rats
# Y = body weight of rat
# Treatment = treat (levels: control, or thiouracil or thyroxine)
# Grouping factor or Unit: subject (the rat)
# Time: weeks
# 
# (10 rats are in control, 7 rats are given thyroxine in water, and 10 rats are given water
# with thiouracil added)



drinkData <- read.table("data/rd.txt", header=TRUE)
head(drinkData)

N <- nrow(drinkData)
drinkData$subject <- factor(paste(rep('R', N), drinkData$subject, sep=''))
#drinkData$Weeks <- factor(paste(rep('W', N), drinkData$weeks, sep=''))
drinkData


# part (a) -----------------------------------------------------------------------------

# Exploratory plots for relation between weight/time for different treatments

# Unit = rat, the subject (grouping factor)
# Time (primary covariate) (NOTE: must be NUMERICAL, never quantitative)
# Treatment: treat (thyroxine ...)
### outer = treat
### Random part: Unit x Time = ~Time | subject part (random effects from how units react over time)
### Fixed part: Treatment x Time = treat * Time


# PLOT 1: 
# usable for identifying (1) between-group variation, and (2) within-group variation

# formula = response ~ primaryCovariate | groupingFactor or ~1 |groupingFactor when 
# no other suitable candidate
drinkData <- groupedData(wt ~ weeks | subject, outer= ~treat, data=drinkData)
plot(drinkData, outer=TRUE)

# Or by ggplot: 
ggplot(data=drinkData, aes(x=weeks, y=wt, color=subject)) + geom_line() + geom_point() +
      facet_wrap(~ treat, ncol=3)


# INTERPRET:  to see structure of ssytematic and random effects, we are plotting profiles 
# for each experimental unit (rat) across time (weeks). 

# (i) linearity in response: response is slightly concave up for control and
# thyroxine group but concave down for thiourcaile group so this group had
# less weight increaseas time went on, compared to others. 

######## (1) Addressing: VARIABILITY AMONGST GROUPS: (fixed effect interaction)

### --- Intercepts among treatments:
# (ii) similar intercepts for all treatment groups. 

### --- Slopes among treatments:
# (III) growth rate (slope) seems steeper for control and thyroxine as time goes on
# compared to thiouracil. Increasing weight gain as time goes on treats 1,3


######## (2) Addressing: 
#   VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# this is sigma.residual (???)

# (IV) Random Intercepts: (Space between lines within treat): variability in 
# Within each treatment group there is variability among subjects since the 
# lines are spaced out. True for control since lines are spaced out, true for
# thiouracile since lines are spaced out ... etc 
# =====> suggests some need for random intercepts model: ~|subject

# (V) Random slopes: (Slopes within treat): the rate of increase in weight over time
# (slope) is similar for all subjects/rats within a particular treatment group. 
# So for thiouracil, see that lines all have similar slope. 
# For control, all lines have similar concave up shape. 
# For thyroxine, half the lines have steeper slope than other half. 
# ====> suggests some need for random slopes: ~Time|subject


# (vi) Fanning out: differences amongst rats at the beginning are preserved throughout
# the trial  (since the lines are spaced similarly over time). True for Control and
# Thiouracil but not for Thyroxine. 



# PLOT 2: 
# usable for identifying (1) between-group variation, and (2) within-group variation


### INTERCEPTS ANS SLOPES 95% CI: 

#### Can visualize the mean response as a weighted average of the individual rat profiles
# provided that the between-rats variance is included in the averaging. 
# Finding linear response for each rat using lmList()
# Can help explain random effects when the model is fitted. 
drink.lmlist <- lmList(wt ~ weeks | subject, data=drinkData)
drink.lmlist

# note: for this to work, do NOT load library(lme4) and nlme at the same time
drink.ci <- intervals(drink.lmlist) 
plot(drink.ci)

#INTERPRET: 
# Intercepts: vary so suggests random intercepts model
# Slopes = lots of variation so suggests random slopes model

## should fit: randintercepts/random slopes model

# using ggplot
'CI.df <- data.frame(intercept=data.frame(drink.ci[,,"(Intercept)"]), 
                    slope= data.frame(drink.ci[,,"weeks"]))
CI.df$subject <- rownames(CI.df)

ints = CI.df$intercept.est.
slopes = CI.df$slope.est.
cidf <- data.frame(Estimate=c(ints, slopes), 
                   Lower = c(CI.df$intercept.lower, CI.df$slope.lower),
                   Upper = c(CI.df$intercept.upper, CI.df$slope.upper),
                   Type=c(rep("intercept", length(ints)), rep("slope", length(slopes))),
                   subject=rownames(CI.df))

ggplot(cidf, aes(y=Estimate, x=subject, color=Type)) + 
      geom_errorbar(aes(ymin=Lower, ymax=Upper)) + 
      geom_line() + geom_point() + facet_wrap(~Type, ncol=2) 
'



## PLOT 3
# usable for identifying (1) between-group(treat) variation (so just fixed effect interaction)

# Interaction plot of week and treatment: 

with(drinkData, interaction.plot(x.factor=weeks, trace.factor=treat, response=wt))

# suggests interaction between treat and week starting from week 2 onwards
# and the thiouracile value. 

# INTERPRET: (sol)
# Shows trend over time for the three treatment groups, averaged over subjects. 
# Rate of increase in weight over time is similar for the two treatment groups
# but is lower for the rats given thiouracile since thiouracil slope is lower. 

# ===> suggests fixed effect interaction: Time * Treat



# STRATEGY: plots 1, 2, 3 suggests a fixed-effects interaction model + random effects
# model using random slopes and intercepts. 

# NOTE: no need to centre weeks since weeks = 0 is a meaningful measure. 



# part (c) ---------------------------------------------------------------------------

# Fitting the fixed effects interaction + randomslopes/intercepts model: 
drink.lme <- lme(wt ~ treat*weeks, random= ~weeks|subject, data=drinkData)
# Fitting fixed effects interaction + random intercepts (only)
drink.intercept.lme <- lme(wt ~ treat*weeks, random = ~1|subject, data=drinkData)

# Comparing the two: 
anova(drink.intercept.lme, drink.lme)
# p-value is small ===> the random slopes term is significant. 
# (OR) if going by AIC, say AIC for random slopes/ints is much less than for intercepts-
# only model. 


# Analyzing fixed-effects part of RandomSlopes/random intercepts model: 

# So getting the output for the random slopes/random intercepts model: 
anova(drink.lme) # shows only fixed effects (between group variation)

# INTERPRET: 
# treat:weeks term has low p-value 
# ===> interaction between treat and weeks is significant 
# ===> change in weight over time (slope) is NOT the same for all treatment groups 
# ===> keep the fixed-effects interaction term. 

summary(drink.lme)

# INTERPRET: 
### --> treatThiouracile:Weeks coeff is significant ==>  
# ===> change in weight over time IS significantly different for (throucail vs control)
### Confirms interaction plot: 

### --> treatThyroxine:Weeks coeff NOT significant ==> change in weight over time is NOT
# significantly different for the (thyroxine vs control) .


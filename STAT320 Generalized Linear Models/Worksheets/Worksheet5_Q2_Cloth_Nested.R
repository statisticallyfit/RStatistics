setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#detach(package:lme4)
#detach(package:nlme)
library(nlme) # lme
library(lme4) # lmer
library(lattice)

options(digits=8)
options(show.signif.stars = FALSE)

# Design: randomized block design (block = the cloth bolt)
# Chemical = 4 levels
# strengh = tensile strengths of cloth after applying the chemical
# bolt = 5 levels of cloth (block)
clothData <- read.table("data/cloth.txt", header=TRUE)
clothData$Bolt <- factor(clothData$Bolt)
clothData$Chemical <- factor(clothData$Chemical)



# Exploratory plots: 

# (1) Horizontal line is the overall mean of strengh. The plot shows the level means
# for each of the factors. 
# Mean strength is highest for Chemical 3. 
# Little difference in mean strength between bolt 1 and bolt 4, and big difference
#  in mean strength between bolt 2 and bolt 3. 
plot.design(strength ~ Bolt + Chemical, data=clothData)



# (2) Interaction plot: 

with(clothData, 
     interaction.plot(x.factor=Chemical, trace.factor=Bolt, response=strength))

# or with ggplot
ggplot(data=clothData, aes(x=Chemical, y=strength, group=Bolt)) + 
      geom_line(size=1, aes(color=Bolt))

# My homemade function: 
interactionPlot(data=clothData, xFactor="Chemical", traceFactor="Bolt", 
                response="strength")

# INTERPRET: change in mean strength (slope of strength) is similar for all Bolts
# as we move from Chemical 1 through to 4, suggesting that interaction
# between bolt and chemical is unlikely. (slight difference in slope from
# chemical 2 to 3 for bolt 5)



# Fit the interaction and no interaction model, and compare

# RANDOM EFFECT: Bolt (since it is the block in this randomzied block design)
# FIXED EFFECT: Chemical (since we want only to analyze specific, fixed levels
# of the chemical and we are not randoml ysampling chemical from a population)

# Usually the block is assignned to be the RANDOM EFFECT  in the analysis. 

cloth.lme <- lme(strength ~ Chemical, random = ~1|Bolt, data=clothData)
cloth.interact.lme <- lme(strength ~ Chemical, random = ~1|Bolt/Chemical, data=clothData)


# TODO: is anova testing the significance of the variance  random effects components? 

anova(cloth.lme, cloth.interact.lme)

# INTERPRET: p-value = 1 so that means the interaction term is far from significant
# Also the non-interaction model has lower AIC of 86.088 versus the 88.088 of
# the interaction model, so the non-interaction model is best. 


# Now fit lmer model to easily see the variance components
cloth.lmer <- lmer(strength ~ Chemical + (1|Bolt), data=clothData)
summary(cloth.lmer)
# sigma.residual = 1.375 (within bolt variation)
# sigma.Bolt = 3.022 (between bolt variation)

# Summary coefficients
summary(cloth.lme)$tTable
# INTERPRET: 
# Sigificant differences in mean strengh between chemical 1 and all other
# chemical levels 2,3,4, and all other levels have higher mean strength
# than chemical 1 since the coefs are positive. 


# Remove intercepts to obtain estimates of means and their standard errors
cloth0.lme <- lme(strength ~ Chemical - 1, random=~1|Bolt, data=clothData)
summary(cloth0.lme)$tTable



# Confidence intervals of fixed effects, random effects, and variance components
intervals(cloth0.lme)
# INTERPRET: 
# see again that fixed effects difference means are significantly above 0, 
# and that Chemical4 - chemical1 interval is marginally above 0, corresponding
# to the marginally significant p-value 0.04 for the Chemical 4 coefficient. 
# So since the interval is marginally above zero, it is very near to being
# non significant. So there is an almost non-signfiicant mean difference
# between chemical 4 and chemical 1. 


# Resetting base level to chemical 3 and test for differences in treatment
# effects (treatment = chemical, the fixed effect)
clothData3 <- clothData
clothData3$Chemical <- relevel(clothData3$Chemical, ref="3")
cloth3.lme <- lme(strength ~ Chemical, random = ~1|Bolt, data=clothData3)

summary(cloth3.lme)$tTable # all differences are significant.

summary(cloth3.lme) # same variance components as for ref = 1 model. 

intervals(cloth3.lme)
# INTERPRET: 
##### Fixed effects: 
# All intervals are below zero, so Chemical 3 is significantly higher in mean 
# strength than each of  chemicals 1,2,4. 
##### Random effects: 
# the confint  for the Bolt random effect is (1.457, 6.26) is above 0 so the 
# between-bolt variation is significantly above 0. 
 #The CI for the residual random effect is (0.921, 2.05) is above 0 so the
# within-bolt variation is marginally significantly above 0. 
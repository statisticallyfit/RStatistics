setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
library(lme4)
library(lattice)

options(show.signif.stars = FALSE)


# part (a) -----------------------------------------------------------------------
# Consructing the data set

Trig <- scan('data/trigl.txt')
Trig

Method <- c(rep("M1", 4*2), rep("M2", 4*2))
Day <- rep(c("D1", "D1", "D2", "D2", "D3", "D3", "D4", "D4"), 2)

trigData <- data.frame(Trig=Trig, Method=Method, Day=Day)
trigData

# part (b) -----------------------------------------------------------------------
# Exploratory plot and summraize information

# Day within method variance component ( for a nested design) 

### Variation among Day within Mehod: little variation in D4 for Method 1 and 2
# but there is larger variation for D2,D3 for Method 1 and for D1, D3 in Method 2. 
# Overall, within each method, there is larger variability in Triglyceride level
# for Day1,2,3 and lower variability in triglyceride level for Day 4. 
bwplot(Trig ~ Day | Method, data=trigData) # Day within method


# Method Variance Component (in a nnested design)

### Variation within Method: If we averaged across Day within each Method, we  see
# that the average eye intensity differs between Methods 1, 2. In particular, 
# the mean triglyceride level for Method 1 is higher than for Method 2. 
# This is the Method variance component. 
bwplot(Trig ~ Method , data=trigData) 


# Suggests maybe interaction between Day and method since slope from D1 to D2
# for M1 is not the same for M2. However the slopes are similar for the other
# levels for Day. 
with(trigData, interaction.plot(x.factor=Day, trace.factor=Method, response=Trig))

#interactionPlot(data=trigData, xFactor='Day', traceFactor='Method', response='Trig')

#groupedTrigData <- groupedData(Trig ~ Method | Day, data=trigData)
#plot(groupedTrigData)
#xyplot(Trig ~ Day, data=groupedTrigData, groups=Method, 
#                      panel=panel.superpose, panel.groups=panel.lmline)

# INTERPRET: 
### Variation among labs: see that lab 4 is different than other labs.
### Variation among batches WITHIN LABS: see that there is little variation among
# batches within labs since L1, L2, L3 seem to have the same variation per batch
# only L4 and L6 seem to have different variation per batch, especially at batch
# B1, B2 for Lab4. 

dotplot(Day ~ Trig | Method, data=trigData, pch=c(1,1,2,2,3,3,4,4),
        strip=FALSE, strip.left=TRUE, layout=c(1,2), cex=1.5, 
        ylab="day within method", xlab = "triglyceride level", jitter.y = TRUE)


# part (c)-------------------------------------------------------------------------

# MODEL: (????)
# Fixed effect: Method (analyzing fixed, particular method not generalizing a population)
# Random effect: Day (a random sample of days taken from a population  of days)
# INTERACTION term: between day and method: is a random effect also.

# Matrix representation: 
# y = X*B + Z1*b + Z2*m + e
# where,
### Random effect Day: b ~ N(0, sigma_Day^2 * I)
### Interaction effect: Day:Method: m ~ N(0, sigma_Day_Method^2 * I)
### residuals: error ~ N(0, sigma^2 * I)
### X = fixed effect (Method) design matrix
### Z1 = random effect (Day) design matrix
### Z2 = interaction random effect (Day:method) design matrix


#  part (d) ----------------------------------------------------------------------------

# notation: Worker/Machine means Worker + (Machine-within-worker) : ~1| Worker/Machine
# notation: seed/fertz means seed + (fert-within-seed) : ~1 | seed/fert

trig.lme <- lme(Trig ~ Method, random = ~1 | Day/Method, data=trigData)
trig.lmer <- lmer(Trig ~ Method + (1|Day/Method), data=trigData)
summary(trig.lmer)
summary(trig.lme)
VarCorr(trig.lme)


# INTERPRET: 
sigma.methodInDay <- 4.85764
sigma.residual <- 3.797038
sigma.day <- 4.533303


# Significance of variance components: 
intervals(trig.lme)
# INTERPRET: 
# No Ci's are significantly different from each other since they each overlap. 




# Fit the non-interaction model

trig.nointeraction.lme <- lme(Trig ~ Method, random = ~1 | Day, data=trigData)
trig.nointeraction.lmer <- lmer(Trig ~ Method + (1|Day), data=trigData)
summary(trig.nointeraction.lmer)
summary(trig.nointeraction.lme)



# part (e) ------------------------------------------------------------------------------

# Which model is preferred: 
anova(trig.nointeraction.lme, trig.lme)
# Going by p-value: the interaction term is not important
# Going by AIC: the interaction model has lower AIC but the delta is small so choose the
# more parsimoniuous model and p-value is in favor of the simpler model too. 


# part (f) -------------------------------------------------------------------------------

# Write the terms in the final model: 

summary(trig.nointeraction.lme)
random.effects(trig.nointeraction.lme)

# fixed effects line: 147.000 - 0.975 * MethodM2
# random effects line: 1.367301*D1 - 7.120095*D2 + 1.954734*D3 + 3.798059

# part (g) -------------------------------------------------------------------------------

# Get the variance components of the final model : 
VarCorr(trig.nointeraction.lme)


# INTERPRET: 
# sigma.residual <- 5.22383
# sigma.day <- 5.39736


# Significance of variance components: 
intervals(trig.nointeraction.lme)
# INTERPRET: 
# No Ci's are significantly different from each other since they each overlap. 


# part (h) -------------------------------------------------------------------------------

# RESEARCH question: do the two methods differ?

# Using the non-interaction model, looking at fixed effects we see that p-value for
# the MethodM2 coeff is 0.0052 so there is a significant difference between M2 and M1. 
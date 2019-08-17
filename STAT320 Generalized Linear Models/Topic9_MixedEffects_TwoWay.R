setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#detach(package:lme4)
#detach(package:nlme)
library(nlme)
library(lme4)
library(lattice)
options(show.signif.stars = FALSE)

# Two-way classification: applies to TWO factors, which we call A, and B
### Use anova when both factors A, B are fixed
### Use mixed effects models when one factor is fixed and the other is random (A or B)

# Example 9.1: -----------------------------------------------------------------

# ONE FIXED and ONE RANDOM EFFECT ----------------------------------------------

# Data: Machines: contains productivity score for each of the six randomly chosen
# workers tested on each of the 3 different machines. 
# Each worker used the machine 3 times, giing 3 replicates for each worker/machine 
# combination. 

data(Machines)
head(Machines)
Machines$Worker
Machines$Machine

attach(Machines)

# Dotplot: 
# (1) shows there is a difference between machines (check the horizontal way)
# since the circles, +, and triangles representing machines A,B,C are spread out
# horizontally along scores 45 -> 70. Machine C generally gets higher scores
# for all workers. 
# (2) Also dotplot shows some differences between workers (look at worker axis
# as the bottom and check the vertical height when looking that way). See
# that worker 3 had higher scores than all workers for all machiens. He is shifted
# higher for all machines. Worker 6 had higher productivity on machine A and lower
# on machine B, contrary to every other worker. 
dotplot(Worker ~ score, groups=Machine, ylab="Worker")

# INTERACTION Plot. 
interaction.plot(x.factor=Machine, trace.factor=Worker, response=score)
# my function
interactionPlot(data=Machines, xFactor=Machine, traceFactor=Worker, response=score)
# INTERPRET: 
# Shows interaction between worker and machine since the slopes of score for
# workers is not the same over machine levels. 
# Productiviy of worker 6 differs from the others. He rates machine B the lowest.


# MODEL: 
# Fixed effect: taken to be MACHINE (analyzing fixed, particular machiens
# A, B, C not generalizing a population)
# Random effect: WORKER (a random sample of workers taken from a population 
# of workers)
# INTERACTION term: between machine and worker: is a random effect also. (rule)

# Matrix representation: 
# y = X*B + Z1*b + Z2*m + e
# where,
### Random effect worker: b ~ N(0, sigma_b^2 * I)
### Interaction effect: worker:Machine: m ~ N(0, sigma_m^2 * I)
### residuals: e ~ N(0, sigma^2 * I)
### X = fixed effect (machine) design matrix
### Z1 = random effect (worker) design matrix
### Z2 = interaction random effect (worker:machine) design matrix

# notation: Worker/Machine = Worker + (Machine-within-worker) is how you
# specify an interaction between worker and machine. 
machine.lme <- lme(score ~ Machine, random = ~1|Worker/Machine)
machine.lme
# not the same thing: 
#machine2.lme <- lme(score ~ Machine, random = ~1|Machine/Worker)
#machine2.lme

# Machine %in% Worker (means: FIXED_EFFECT %in% RANDOM_EFFECT)
summary(machine.lme)

# See: 
# within group variation = residual var
sigma.residual <- 0.9615771 
# between group (worker) variation (worker variance)
sigma.worker <- 4.78105     
# between group (machine:worker) variation (interaction variance)
sigma.machine_worker <- 3.729532 


# TESTING: if interaction term is significant.

# Fit the model without interaction
machine.nointeraction.lme <- lme(score ~ Machine, random = ~1|Worker)

# COMPARE USING ANOVA: prerequisite: the models must have the same FIXED EFFECTS. 

# Has both AIC, BIC, and L.Ratio
anova(machine.nointeraction.lme, machine.lme)
# INTERPRET: 
# Interaction model has lower AIC = 227 than main model AIC = 296 ===> 
# ===> interaction is better. 
# MEANING of significant interaction: differences among machines vary with each
# worker.

# NOTSE: 
### (1) to compare mixed models, with different FIXED but same RANDOM effects, use 
# ML estimation not REML estimation of coeffs. 
# But final model coefs should be re-obtained using REML. (specify method = "ML")

### (2) to use anova to compare mixed models, the fixed terms must be the same
# while the random terms can be diferent. 

### (3) Note that for a balanced design, the predicted main fixed effects
# are the same between the mixed model and the regular lm model: 
machines.lm <- lm(score ~ Machine * Worker, data=Machines)
# Coefs in regular lm model
summary(machines.lm)
# coefs in LME model
summary(machine.lme)
# see that the fixed effect coefs MachineB = 7.99 and MachineC = 13.9 are the same
# for both models. 
# BUT the std errors and thus test statistics differ because there are additional
# variance components in the lme compared to in the lm. 

# Checking the fixed effects coefs are the not same anymore when the design is
# unbalanced:
detach(Machines)
mac = Machines[-c(2,3,6,8,9,12,19,20,27,33),]

# showing the data are now unblanaced: 
table(mac$Machine, mac$Worker)
# Old data is balanced: (same number of counts per cell in predictor variables)
table(Machines$Machine, Machines$Worker)

attach(mac)

mac.unbalanced.lm <- lm(score ~ Machine * Worker)
mac.unbalanced.lme <- lme(score ~ Machine, random = ~1|Worker/Machine)
summary(mac.unbalanced.lm)
summary(mac.unbalanced.lme)



# Refitting ineraction model using the ML method
detach(mac)
attach(Machines)
machines.ML.lme <- lme(score ~ Machine, random = ~1|Worker/Machine, method="ML")
AIC(machines.ML.lme)
AIC(machines.lm)

AIC(mac.unbalanced.lm)
AIC(mac.unbalanced.lme) # worse, is higher than for lm

# just observe AIC differs for a model fitted using ML vs REML



### RESIDUAL PLOTS ---------------------------------------------------------------

# Residuals plots for fixed effect (standardized residuals)

# Residuals vs fitted
plot(machine.lme) # residuals vs fitted
qqnorm(machine.lme)

# Residuals vs predictors
#df <- data.frame(machine.lme$residuals)
#plot(Machines$Machine, df$fixed)
plot(Machines$Machine, machine.lme$residuals[,1])

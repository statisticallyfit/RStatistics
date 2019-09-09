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

# data manipulation (for augment())
library(broom)
library(broom.mixed)

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
detach(Machines)

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
with(Machines, dotplot(Worker ~ score, groups=Machine, ylab="Worker"))


# INTERACTION Plot. 
with(Machines, interaction.plot(x.factor=Machine, trace.factor=Worker, response=score))
# my function
interactionPlot(data=Machines, xFactor='Machine', traceFactor='Worker', response='score')
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
machine.lme <- lme(score ~ Machine, random = ~1|Worker/Machine, data=Machines)
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

#summary(machine.lmer)


# TESTING: if interaction term is significant.

# Fit the model without interaction
machine.nointeraction.lme <- lme(score ~ Machine, random = ~1|Worker, data=Machines)

# COMPARE USING ANOVA: prerequisite: the models must have the same FIXED EFFECTS. 

# Has both AIC, BIC, and L.Ratio
anova(machine.nointeraction.lme, machine.lme)
# INTERPRET: 
# Interaction model has lower AIC = 227 than main model AIC = 296 ===> 
# ===> interaction is better. 
# MEANING of significant interaction: differences among machines vary with each
# worker.

# NOTES: 
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
#detach(Machines)
mac = Machines[-c(2,3,6,8,9,12,19,20,27,33),]

# showing the data are now unblanaced: 
table(mac$Machine, mac$Worker)
# Old data is balanced: (same number of counts per cell in predictor variables)
table(Machines$Machine, Machines$Worker)

#attach(mac)
#detach(mac)

mac.unbalanced.lm <- lm(score ~ Machine * Worker, data=mac)
mac.unbalanced.lme <- lme(score ~ Machine, random = ~1|Worker/Machine, data=mac)
summary(mac.unbalanced.lm)
summary(mac.unbalanced.lme)



# Refitting ineraction model using the ML method
#detach(mac)
#attach(Machines)
machines.ML.lme <- lme(score ~ Machine, random = ~1|Worker/Machine, method="ML", data=Machines)
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

# Boxplots of residuals by machine and worker
plot(Machines$Machine, machine.lme$residuals[,1])
plot(Machines$Worker, machine.lme$residuals[,1])

# Boxplot of residuals vs machine --- HELP but why is this different than above?
aug <- augment(machine.lme, data=Machines)
ggplot(aug,aes(Machine, .resid))+
      geom_boxplot()+coord_flip()


# Equivalent: residuals are in the third column of fitted variable
plot(machine.lme)
plot(machine.lme$fitted[,3], machine.lme$residuals[,3])
machine.lme$fitted # the fitted values for fixed, Worker and Machine variables
machine.lme$residuals # the residuals for fixed, Worker, and Machine variables




### Exercise: Beet data -----------------------------------------------------------

beetData <- read.table("data/beet.txt", header=TRUE)
head(beetData)
beetData$fert <- factor(beetData$fert)
beetData$fert
beetData$seed <- factor(beetData$seed)
beetData$plot <- factor(beetData$plot)

# Data: 3x4 factorial study was designed to study effects of the
# FIXED FACTOR (fertilization method: fert (3 levels)) and the 
# RANDOM FACTOR (seeding rate: seed (4 levels)). 
# There were 3 replications per treatment. 
# Yield = response variable

# (a) random effect: seeding rate: want to extrapolate to population. Means
# we study random sample not the seeding rate atfixed or specific levels. 
# Fixed effect: fertilization method, means we study fertilization at 3 specific
# levels and is not considered a random sample, just fixed. 

# (b) estimating variance components of all random effects

# notation: Worker/Machine means Worker + (Machine-within-worker)
# ~1| Worker/Machine

# notation: seed/fertz means seed + (fert-within-seed) 
# Means how to specify an interaction between fert and seeding rate. 
beet.lme <- lme(yield ~ fert, random = ~1 | seed/fert, data=beetData)
summary(beet.lme)

# INTERPRET: 
# within group variation = residual var
sigma.residual <- 0.240682
# between group (seed) variation (seed variance)
sigma.seed <- 0.7032103     
# between group (fert:seed) variation (interaction variance)
sigma.fert_seed <- 0.1802853 


# TESTING: if interaction term is significant.

# Fit the model without interaction
beet.nointeraction.lme <- lme(yield ~ fert, random = ~1|seed, data=beetData)

anova(beet.nointeraction.lme, beet.lme)
# INTERPRET: 
# Interaction model has lower AIC so it is better than main effects model. 
# MEANING of this significant interaction: differences among fertilizer vary
# with each seeding level. 

#interaction.plot(x.factor=Machine, trace.factor=Worker, response=score)
with(beetData, interaction.plot(x.factor=fert, trace.factor=seed, response=yield))



### 9.5 COMPONENTS OF A MIXED MODEL ---------------------------------------------
# Understanding: design matrices, predictions, residuals

 #Run the machines example again using LMER method in library(lme4)

# (1) DESIGN MATRICES: 
# y = XB = Z1 * b + Z2*m + e
# b ~ N(0, sigmab^2 * I)
# m ~ N(0, sigmaM^2 * I)
# e ~ N(0,sigma^2 *I)

# where,
### Random effect worker: b ~ N(0, sigma_b^2 * I)
### Interaction effect: worker:Machine: m ~ N(0, sigma_m^2 * I)
### residuals: e ~ N(0, sigma^2 * I)
### X = fixed effect (machine) design matrix
### Z1 = random effect (worker) design matrix
### Z2 = interaction random effect (worker:machine) design matrix

# Producing design matrices
machine.lmer <- lmer(score ~ Machine + (1|Worker/Machine), data=Machines)
# compare to lme
# machine.lme <- lme(score ~ Machine, random = ~1|Worker/Machine)

machine.lmer
summary(machine.lmer)

# Get design matrix
getME(machine.lmer, "X")
m = getME(machine.lmer, "Z")
head(Machines)

# -> column 1 in the Z design matrix (actually a compbination of Z1 worker and 
# Z2 interaction effect) corresponds to random effect worker 1
# on machine A (denote this as A.1). Can see this as observations 1-3 in the rows.

# But actually col 1 corresponds to random effect worker 6 on machineA (A.6)
# because of how our object is set up - different than in lec notes. 
m@Dimnames # obs 1-3, is Machine=A, worker = 6
m[1:3,]

m@Dimnames # obs 4-6: is Machine = A, worker = 2
m[4:6,] # worker 2 so ones are in second position

m@Dimnames # obs 7-9: is Machine = A, worker = 4
m[7:9,] # worker 4 so ones are in fourth position


summary(machine.lme)
random.effects(machine.lme)

# So the model can be written: 
# mu|b = 52.36 + 7.967*MB + 13.917*MC - 0.75 * MA:W1 + ... + 1.499*MB*W1 + ... + 2.48 MC:W6
#   + 1.045 * W1 + ... + -7.51*W6


## (2) PREDICTED VALUES: 
# To get the coefficient analysis below, need to use both: 
predict(machine.lme, level=0:2)
# and
random.effects(machine.lme)

# NOTE: the level=0:2 in the predict command specifies the types 
# of predictors in the RANDOM PART of the model (?): fixed, random, and interaction

# LEVELS:

# --> level = 0: the population level (the fixed effects (Machines) averaged over 
# random effect (worker))
# --> level = 1: the individual worker (random effect)
# --> level = 2: the worker machine interaction

# OUTPUT:

##### ---> predict.fixed: corresponds to level = 0. 
# Predict.fixed = prdicted RESPONSE for each FIXED EFFECT averaged over
# the RANDOM effect. (So here, predicted score for each machine avged over worker)
# Output: 
# ------ 52.356 = mean score for Machine A, averaged over worker
# see is averaged over worker since 1/A, 2/A, ... 6/A are all the same, for 
# all worker values: 1-6
# ------ 60.322 = mean score for machine B, avged over worker. 
# ------ 66.27 = mean score for machine C, avged over worker. 

##### ---> predict.RANDOM (predict.Worker) = level 1. The indiivudal random effect (worker) predictions. 
# Output: 
# ------- predicted value of worker 1 on machine A: 
# = mean for machine A + COEF for worker 1 
# = 52.355 + 1.045 
# = 53.4
# ------- predicted value of worker 2 on machine A: 
# = mean for machine A + COEF for worker 2
# = 52.355 + -1.37585675
# = 50.97914
# ...
# ------- predicted value of worker 6 on machine C: 
# = mean for machine C + COEF for worker 6
# = 66.27222 + -7.51429458
# = 58.75793


##### --> predict.FIXED: (here predict.Machine), corresponds to 
# the level = 2: the worker machine interaction
# Output: 
# -------- predicted value of interaction of worker 1 on machine A: 
# = mean for machine A + COEF of worker 1 + COEF of interaction (worker1 on Machine A)
# = 52.35556 + 1.04454677 + -0.7501469
# = 52.64996
#
# ------- prediced value of interaction of worker 3 on machine B: 
# = mean for machine B + COEF of worker 3 + COEF interact (Worker 3 on machine B)
# = 60.32222 + 5.36077966 + 2.2993809
# = 67.98238



### (3) RESIDUALS: 
machine.lme$residuals

# predicted fixed effect for machine A avged over worker = 52.356
predict(machine.lme, level=0)
# residual for fixed effect for observation 1 = -0.3556
machine.lme$residuals[1,]
# observed value from original data set for observation 1 = 52
head(Machines, 1)

# --> residual for fixed effect of machine A avged over worker = obs - pred
# = 52 - 52.356 = -0.356, as expected
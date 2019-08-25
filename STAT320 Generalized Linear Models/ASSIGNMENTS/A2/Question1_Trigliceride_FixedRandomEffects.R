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
# Fixed effect: Day (analyzing fixed, particular days not generalizing a population)
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
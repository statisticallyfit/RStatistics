setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
library(lme4)
library(lattice)

options(show.signif.stars = FALSE)


### EXERCISE: FLIES --------------------------------------------------------------

# PROBALEM DESCRIPTION for Fly Data: 
# Three male flies chosen at random is mated with four different
# females chosen at random (so total of 12 females chosen at random). Two offspring
# are born from each mating and the intensity of their eyes is measured (response). 

# Observe: (1) the nested part is "female within male" for two reasons: 
# ---> (reason 1) the females are mated with each of the 3 male flies
# ---> (reason 2) the females differ in kinds of females from one male to another. 

# Observe: (2) the factors are both RANDOMLY sampled so they are random effects.

#### MODEL DESCRIPTION for the Analyte Data: 
# --> Variation in (y) eye intensity is a combination of the
# three variances due to: (1) measurements, (2) females, and (3) males 
# --> the three components of variations are due to the distribution of: 
# (1) male means (i) around overall means, 
# (2) female means (j) around the ith male mean, 
# and (3) individual measurements (k) around the jth female mean. 

# Observation: Y_ijk, error_ijk
# male mean = alpha_i
# female mean = beta_j(i)           (female j within male i)

# The model is (for A = Male, B = Female,  both random):

# Y_ijk | Male_i, Female_j(i) ~ Normal(mu_ij | Male_i, Female_j(i), sigma^2)
# mu_ij| Male_i, Female_j(i) = mu + Male_1 + Female_j(i)
#
# error_ijk = Y_ijk - mu_ij
# error_ijk ~ Normal(0, sigma^2)
#
# Female_j(i) ~ Normal(0, sigma_Female ^2)
# Male_i ~ Normal(0, sigma_Male^2)
# 
# cov(Male1, Female_j(i)) = 0
# cov(Male1, error_ijk) = cov(Female_j(i), error_ijk) = 0

# ... so the factors are independent of errors? 


### DIFFERENCE BETWEEN NESTED vs. CROSSED DESIGN: 
# Nested design: DIFFERING females are mated with each of the males. 
# Crossed design: the SAME females would be mated with each of the males. 

flyData <- read.table("data/flies.txt", header=TRUE)
flyData
flyData$female <- factor(flyData$female)
flyData$male <- factor(flyData$male)
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

# Replication = 2 ( means 2 females of each kind within one male kind)

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
N <- nrow(flyData)
flyData$female <- factor(paste(rep('F', N), flyData$female, sep=''))
flyData$male <- factor(paste(rep('M', N), flyData$male, sep=''))


 # Exploratory plots

### For each female, regardless of male, there is little variability in 
# eye intensity between the two offspring. (amongst females?)
# This is the RESIDUAL VARIANCE COMPONENT. 
#### (?) bwplot(eye ~ female, data=flyData)

### Variation among males: If we averaged across females within each male, we  see
# that the average eye intensity differs from male to male. In particular, the mean
# eye intensity of offspring from male 2 would be lower than for male 1, male 3. 
# See that M2 is opposite to M3
bwplot(eye ~ male, data=flyData)
# This is the MALE VARIANCE COMPONENT.

### Variation among females within males: little variation in females for M3, M2
# and for M1 except there is alrger F2 variation in eye intensity for M1. 
# This means that within each male, there is little variability amongst eye
# intensitities from female to female. So F1, F2 ... F4 have all the same
# little variation, within each male box. 
# This is the FEMALE WITHIN MALE VARIANCE COMPONENT. 
bwplot(eye ~ female | male, layout=c(3,1),data=flyData)


# Plot 2
dotplot(female ~ eye | male, data=flyData, pch=c(1,1,2,2,3,3,4,4),
        strip=FALSE, strip.left=TRUE, layout=c(1,3), cex=1.5, 
        ylab="female within male", xlab = "eye intensity", jitter.y = TRUE)


# part (c) -----------------------------------------------------------------------
# Fit the mixed effects nested model

# female within male: Female_j(i)
eye.lme <- lme(eye ~ 1, random = ~1|male/female, data=flyData)
summary(eye.lme)

sigma.male <- 4.20786
sigma.femaleWithinMale <- 9.74385
sigma.residual <- 1.1409052

c(sigma.male, sigma.femaleWithinMale, sigma.residual)^2

VarCorr(eye.lme)

# INTERPRET: 
# (1) higher female-within-male variation than male variation and residual variation
# (2) higher within-male variation than residual variation
# +++ Greatest source of variability in eye intensity comes from the females. 


# Proportion of variability in EYE (response) explained by /due to females: 
# (repeatability)
totalVar <- (sigma.male^2 + sigma.femaleWithinMale^2 + sigma.residual^2)
propVarDueToFemales = sigma.femaleWithinMale^2 / totalVar
propVarDueToFemales
# INERPRET: 83.3% of variability in eye intensity of offspring is due to 
# the females. 


# Checking significance of variance components
intervals(eye.lme)
# INTERPRET: 
# sigma.male CI = (0.38, 45.7)
# sigma.female(male) CI = (6.12, 15.5)
# sigma.residual CI = (0.76, 1.70)

# (1) femaleWithinMale Ci is contained within the male CI so the sigma.fmaleWithinMale
# is NOT significantly higher than sigma.male. 
# (2) femaleWithinMale Ci is entirely above the sigma.residual CI so sigma.femWithinMale
# is significantly higher than sigma.residual.
# (3) sigma.residual and sigma.male Ci's are overlapping so these sigmas are not
# significantly different from each other. Within-male variation (sigma.male)
# is NOT significantly higher than sigma.residual. 
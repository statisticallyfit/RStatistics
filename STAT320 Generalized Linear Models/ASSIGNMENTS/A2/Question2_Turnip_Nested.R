setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
library(lme4)
library(lattice)

options(show.signif.stars = FALSE)


# (1) Type of model: Nested: leaves selected within plant (leaves nested within plant)
# (2) fixed or random? Both leaves and plants are random effects since they were randomly
# sampled and so can be generalized to a population. Not just studying their fixed levels. 



# PROBALEM DESCRIPTION for Turnip Data: 
# Four turnip plants are chosen at random. Three leaves are randomly sampled from
# each plant. 
# Then two samples of 100 mg were taken from each leaf. 
# Response: the calcium content of the two samples is measured. 

# Replication = 2 ( means 2 leaves of each kind within one turnip of each kind)

# Observe: (1) the nested part is "leaf within turnip" for two reasons: 
# ---> (reason 1) the leaves are taken from each of the four turnips.
# ---> (reason 2) the leaves differ in levels from one turnip to another (due to random samplling?)

# Observe: (2) the factors are both RANDOMLY sampled so they are random effects.

#### MODEL DESCRIPTION for the Turnip Data: 
# --> Variation in (y) calcium content is a combination of the
# three variances due to: (1) measurements, (2) leaves, and (3) turnips 
# --> the three components of variations are due to the distribution of: 
# (1) turnip means (i) around overall means, 
# (2) leaf means (j) around the ith turnip mean, 
# and (3) individual measurements (k) around the jth leaf mean. 

# Observation: Y_ijk, error_ijk
# turnip mean = alpha_i
# leaf mean = beta_j(i)           (leaf j within turnip i)

# The model is (for A = Turnip, B = Leaf,  both random):

# Y_ijk | Turnip_i, Leaf_j(i) ~ Normal(mu_ij | Turnip_i, Leaf_j(i), sigma^2)
# mu_ij| Turnip_i, Leaf_j(i) = mu + Turnip_1 + Leaf_j(i)
#
# error_ijk = Y_ijk - mu_ij
# error_ijk ~ Normal(0, sigma^2)
#
# Leaf_j(i) ~ Normal(0, sigma_Leaf ^2)
# Male_i ~ Normal(0, sigma_Turnip^2)
# 
# cov(Turnip_1, Leaf_j(i)) = 0
# cov(Turnip_1, error_ijk) = cov(Leaf_j(i), error_ijk) = 0




### DIFFERENCE BETWEEN NESTED vs. CROSSED DESIGN: 
# Nested design: DIFFERING leaf levels are taken from each of the turnips. 
# Crossed design: the SAME leaf levels are taken from each of the turnips. 

turnipData <- read.table("data/turnip.txt", header=TRUE)
turnipData
N <- nrow(turnipData)
turnipData$leaves <- factor(paste(rep('L', N), turnipData$leaves, sep=''))
turnipData$plants <- factor(paste(rep('T', N), turnipData$plants, sep=''))


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





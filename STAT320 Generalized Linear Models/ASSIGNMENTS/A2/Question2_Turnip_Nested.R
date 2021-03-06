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
# Turnip_i ~ Normal(0, sigma_Turnip^2)
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

# RESIDUAL VARIANCE COMPONENT: 
### Averaging over turnip there is some variation across leaves since
# the boxplots are spread a bit apart in height from each other. 
# No leaf has significantly different calcium than any other leaf, since the boxplots
# are overlapping. 
bwplot(ca ~ leaves, data=turnipData)
ggplot(turnipData, aes(x=leaves, y=ca, color=leaves)) + geom_boxplot(size=1)

# PLANT VARIANCE COMPONENT: 
### Variation among plants: If we averaged across leaf within each turnip plant, we  see
# that the average calcium content differs from turnip to turnip. In particular, the mean
# calcium content of leaves from Turnip2 is lower than for all other turnips: T1,T3,T4.
# Also T2 is not overlapping with any other boxplot so leaves from T2 have 
# significantly lower calcium than other turnips. Turnip4 seems to have highest calcium
# but is not significantly higher than in T1, T3. 
bwplot(ca ~ plants, data=turnipData)
ggplot(turnipData, aes(x=plants, y=ca, color=plants)) + geom_boxplot(size=1)

# LEAF WITHIN TURNIP VARIANCE COMPONENT: 
### Variation among leaves within turnips: Within each turnip plant, there is 
# noticeable variation in leaf calcium content because all the leaf boxplos are
# well-separated from other. 
bwplot(ca ~ leaves | plants, layout=c(4,1),data=turnipData)

ggplot(turnipData, aes(x=leaves, y=ca, color=leaves)) + geom_boxplot(size=0.7) + 
      facet_grid(. ~ plants) #+ 
#      theme(legend.title=element_text(size=20), 
#            legend.text=element_text(size=17), 
#            plot.title=element_text(size=20), 
#            axis.title.x=element_text(size=20),
#            axis.title.y=element_text(size=20), axis.text=element_text(size=15))
#bwplot(ca ~ leaves | plants, layout=c(2,2),data=turnipData)



# Plot 2
# note: pch = repeat each number as many times as there are replications
# and the id of the number oges up to number of levels in the y ~
dotplot(leaves ~ ca | plants, data=turnipData, pch=c(1,1,2,2,3,3),
        strip=FALSE, strip.left=TRUE, layout=c(1,4), cex=1.5, 
        ylab="leaf within turnip", xlab = "calcium content", jitter.y = TRUE)


# part (b) --------------------------------------------------------------------------

# # Fit the mixed effects nested model with both factors random. 

# leaf within turnip: Leaf_j(i)
calcium.lme <- lme(ca ~ 1, random = ~1|plants/leaves, data=turnipData)
summary(calcium.lme)

calcium.nonest.lme <- lme(ca ~ 1, random = ~1 | plants, data=turnipData)

anova(calcium.nonest.lme, calcium.lme)

# The nested term is highly significant since AICdelta = 27 = large

# part (c) ------------------------------------------------------------------------

# Estimate variance components 

VarCorr(calcium.lme)

sigma.plants <- 0.6043356
sigma.leafWithinPlants <- 0.4013232
sigma.residual <- 0.08157314

c(sigma.plants, sigma.leafWithinPlants, sigma.residual)^2


# INTERPRET: 
# (1) lower leaf-within-plants variation (0.40) than plant variation (0.60) 
# and residual variation (0.08)
# (2) higher leaf-within-plant (0.40) variation than residual variation (0.08)
## Greatest source of variability in calcium content comes from the plants. 


# Checking significance of variance components
intervals(calcium.lme)
# INTERPRET: 
# sigma.plants CI = (0.24, 1.52)
# sigma.leaf(plants) CI = (0.2433, 0.66179)
# sigma.residual CI = (0.055, 0.1217)

# (1) leafWithinPlants Ci is contained within the plants CI so the sigma.plants is
# NOT significantly different than sigma.leafWithinPlants
# (2) leafWithinPlants Ci is entirely above the sigma.residual CI so 
# sigma.leafWithinPlants is significantly higher than sigma.residual. 
# (3) sigma.residual CI is completely below the sigma.plants CI so the
# sigma.plants is significantly higher than signa.residual. 


# part (d) ---------------------------------------------------------------------

# Find Variance of a single observation: 

# Variance of a single observation = sum of all the variances
# V(Y_ij) = sigma_random^2 + sigma_residual^2 + sigma_interaction^2
VarCorr(calcium.lme)

varSingleObs <- sigma.plants^2 + sigma.leafWithinPlants^2 + sigma.residual^2
varSingleObs
# so variance of a single observation is 0.533


#sigma.leafWithinPlants/varSingleObs


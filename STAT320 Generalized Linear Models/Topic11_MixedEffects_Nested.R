setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
library(lme4)
library(lattice)

options(show.signif.stars = FALSE)

# SO FAR: the two-factor designs have every level of the first factor occurring with
# every level of the second factor. (crossed design)
# EXAMPLE: want to examine effect of applying nitrogen at 6 different levels
# on the yield of 3 varieties of wheat ==> 18 possible treatment combinations. 
# This is a crossed design. 


# But now...

# Nested design: there are levels of a factor nested within levels of another factor. 
# Nested designs are hierarchical where the hierarchies are the 
# different levels of sampling. 
# EXAMPLE: Suppose we know in advance that the three wheat varieties react 
# differently to nitrogen levels ; variety 1 needs low nitrogen (N = 1 or 2), 
# variety 2 needs intermediate level of nitrogen (N = 3 or 4), and variety 3 needs
# high levels of nitrogen (N = 5 or 6 units). 
# Observe: there are 6 nitrogen levels though only 2 levels are being applied to each 
# variety of wheat (instead of all 6 levels being applied to each variety) 
# so that only 6 combinations of treatments are being considered (instead of 18)
# Then the nitrogen factor is nested within the Variety factor. 


# DEFINITION: # Nested design:  ----------------------------------------------------

# Y_ijk ~ Normal(mu_ij, sigma^2)
# E(Y_ijk) = mu_ij = mu + alpha_i + Beta_j(i)
# i = 1,2,... a
# j = 1,2 ... b
# k = 1,2 .... n
# notation j(i) means the jth level of B is nested within the ith level of A. 

# If A and B are both random then can write: 
# alpha_i ~ Normal(0, sigma_A ^2)
# beta_j(i) ~ Normal(0, sigma_B(A)^2)
# error_ijk ~ Normal(0, sigma^2)

# --> levels of factor B are nested within levels of A. Denoted by B(A). 
# --> the same number of levels of factor B are applied to each level of factor A
# --> the levels of B that are applied DIFFER between levels of A. 
# --> can have both factors fixed or random effects. 
# ---> factor A has (a) levels and factor B has (b) levels and there are 
# n replicates of each treatment combination. 

# ----------------------------------------------------------------------------------

# Example when factors are fixed: the wheat/nitrogen example

# Example when factors are random: want to examine characteristics of the offspring
# of bulls of a particular breed of cattle. Each bull is mated with a number of dams 
# and the required characteristic is measured on the offspring. 
# The dams are nested within bulls since DIFFERENT dams are mated with each bull. 


# Example 11.3: --------------------------------------------------------------------


#### PROBLEM DESCRIPTION for the Analyte Data: 
# Seven specimanes were sent to 6 laboraatories in 3 separate batches and each 
# were analyzed for an unnamed Analyte. Each analysis was duplicated. 


#### MODEL DESCRIPTION for the Analyte Data: 
# --> Variation in concentration of the analyte (Y) is a combination of the
# three variances due to: (1) measurements, (2) batches, and (3) laboratories. 
# --> the three components of variations are due to the distribution of: 
# (1) lab means (i) around overall means, (2) batch means (j) around the ith lab mean, 
# and (3) individual measurements (k) around the jth batch mean. 

# Observation: Y_ijk, error_ijk
# Lab mean = alpha_i
# Batch mean = beta_j(i)           (batches j within labs i)

# The model is (for A = Lab, B = batch both random):

# Y_ijk | Lab_i, Batch_j(i) ~ Normal(mu_ij | Lab_i, Batch_j(i), sigma^2)
# mu_ij| Lab_i, Batch_j(i) = mu + Lab_1 + Batch_j(i)
#
# error_ijk = Y_ijk - mu_ij
# error_ijk ~ Normal(0, sigma^2)
#
# Batch_j(i) ~ Normal(0, sigma_Batch ^2)
# Lab_i ~ Normal(0, sigma_Lab^2)
# 
# cov(Lab1, Batch_j(i)) = 0
# cov(Lab1, error_ijk) = cov(Batch_j(i), error_ijk) = 0

# ... so the factors are independent of errors? 



# batch variable = levels 1,2,3
# laboratory variable = 1,2,3,4,5,6
# Subject: specimens
# Specimens are sent to 6 laboratories in 3 separate batches and each analyzed for 
# an unnamed Analyte. 
# Each analysis is duplicated (replication = 2)
# Goal of study: assess components of variation in cooperative trials. 
# The factors laboriatories and batches were random. 

analyteData <- read.table("data/analyte.txt", header=TRUE)
analyteData
analyteData$lab
analyteData$batch




# Exploratory plots

bwplot(conc ~ batch | lab, data=analyteData)
# INTERPRET: 
### Variation among labs: see that lab 4 is different than other labs.
### Variation among batches WITHIN LABS: see that there is little variation among
# batches within labs since L1, L2, L3 seem to have the same variation per batch
# only L4 and L6 seem to have different variation per batch, especially at batch
# B1, B2 for Lab4. 

dotplot(batch ~ conc | lab, data=analyteData, pch=c(1,1,2,2,3,3),
        strip=FALSE, strip.left=TRUE, layout=c(1,6), cex=1.5, 
        ylab="batch within lab", xlab = "concentration", jitter.y = TRUE)
# INTERPRET: same as for bwplot. The crosses represent BATCH 3, circles = BATCH 1, 
# and triangles = BATCH 2. Lab 4 has highest concentration levels for B1, B2, B3
# compared to all the other labs. 
### Variation among labs: so lab 4 is different than others. 
### Variation among batches WITHIN LABS: similar since the circles, crosses, and 
# triangles are near each other for all labs, except for labs 4 and 6. 
# For lab 4: the batch 1 has higher levels of concentration than other B2, B3. 
# For lab 6: the batch 3 has higher concentration levels than B2, B1 for lab 6
# but not higher than all batches in lab 4. 



# Fit the nested model: batch within labs: B_j(i)
analyte.lme <- lme(conc ~ 1, random = ~1|lab/batch, data=analyteData)
summary(analyte.lme)

sigma_lab = 0.2453
sigma_residual = 0.079355
sigma_batch_lab = 0.0732  # batch within lab

c(sigma_lab, sigma_batch_lab, sigma_residual)^2

VarCorr(analyte.lme)

# INTERPRET: 
# Higher variability amongst labs (sigma_lab^2 = 0.06) than among the 
# batches nested in labs (sigma_batch(lab)^2 = 0.005)



# TESTING IF THE NESTED TERM (batch within lab) IS SIGNIFICANT: 
analyte.nonnested.lme <- lme(conc ~ 1, random = ~1 | lab, data=analyteData)
anova(analyte.nonnested.lme, analyte.lme)
# INTERPRET: 
# p-value is marginally significant so nested term can be useful. 
# The AIC says hat the nested term is important since the nested model has
# loweer AIC of -34 compared to the non-nested model with AIc = -32. 
# However the difference (delta) = 2 so the simpler model without the nested
# term may be preferred. 




# 95% CONFIDENCE INTERVALS for the variance components: 
intervals(analyte.lme)
# INTERPRET: 

# (1) All the variance components are significantly higher than 0 since all the
# interval CI's are above 0. 

# (2) the Ci for sigma_batch is a bit overlapping sigma_lab CI so the sigma_lab
# and sigma_batch variance components are not significantly different. 
# So the variation amongst labs is NOT significantly higher than the variation
# amongst batches nested in labs. 

# (3) CI for sigma_lab is higher than CI for sigma_residual so variation amongst
# labs is significantly higher than variation within groups. 

# TODO: what is groups here?


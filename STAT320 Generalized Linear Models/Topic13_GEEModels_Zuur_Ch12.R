setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#library(nlme)
#library(lme4)
#detach(package:lme4)
library(lattice)
library(geepack)

options(show.signif.stars = FALSE)

# CALIFORNIA BIRD DATA: ---------------------------------------------------------------

# Time series of several water bird species recorded in California rice fields. 
# Main goals were to determine whether flooding fields after harvesting results in 
# greater use by aquatic birds, whether different methods of manipulating the straw 
# in conjunction with flooding influences how many fields are used, and whether the 
# depth that thefields are flooded to is important.

# Counts were made during winter surveys at several fields. Here, we only use
# data measured from one winter (1993–1994), and we use species richness to summarise 
# the 49 bird species recorded. The sampling took place at multiple sites, and
# from each site, multiple fields were repeatedly sampled. Here, we only use one site
# (called 4mile) for illustrative purposes. 

data("RiceFieldBirds")

riceData <- RiceFieldBirds
head(riceData)

riceData$Richness <- rowSums(riceData[, 8:56] > 0)
riceData$factorField <- factor(riceData$FIELD)

# xyplot of species richness plotted against time (expressed in two-weekly periods).
# Each panel represents a different field. 
xyplot(Richness ~ Time | factorField, data=riceData, panel = function(x,y){
      panel.grid(h = -1, v=2)
      panel.points(x,y, col=1)
      panel.loess(x, y, col=1, lwd=2)
})

# INTERPRET: 
# There are 11 fields in this site, and each
# field was repeatedly sampled; see Fig. 12.1. Note that there is a general decline in
# bird numbers over time. One of the available covariates is water depth per field, but
#water depth and time are collinear (as can be inferred from making an xyplot of
# depth versus time for each field), so we avoid using them together as covariates in
# the models.



# GOAL of study: explain the richness values as a function of dpeth and management
# fefects. 
# Response = count (poisson glm)
##### OFFSET: need offset because original data were dnesities; numbers per field, and
# the sizes of the fields are different, so log of size of field is offset variable. 
# X1 = depth (quadratic by past experience)
# X2 = management

riceData$LA <- log(riceData$AREA)
riceData$factorSptreat <- factor(riceData$SPTREAT)
riceData$DEPTH2 <- riceData$DEPTH ^2 




#### OWL DATA ------------------------------------------------------------------------

# Data: 
### Response = sibling negotiation (number of calls made by all offspring in absense of
# parents counted during 30-second time intervals before arrival of parent
# divided by number of siblings)
# So response: 
# Y_it = NCalls_it at time (t) in nest (i)
# NCalls_it ~ Pois(mu_it)
### Predictors: 
# Gender of parent = male or female
# Food treatment = deprived or satiated
# Arrival time of parent to nest: 21 to 30 (9:00 pm to 6:00 am)
# offset: number of nestlings

data("Owls")
head(Owls)
owlData <- Owls
cs = colnames(owlData)
cs[5] <- "NCalls"
colnames(owlData) <- cs
owlData$logBroodSize <- log(owlData$BroodSize)

# GOAL OF STUDY: number of calls of nestlings affected by satiation, arrival time?



### DEER DATA --------------------------------------------------------------------------

# Data: 
# Response = presence absense of parasite in deer
# Y_it = 1 if parasite is found in animal (j) at farm(i) at time (t)
# Y_it = 0 if parasite is NOT found in animal (j) at farm (i) at time (t)
# Y_it ` Binomial(1, p_it)  (bernoulli)
# --> p_ijt = probability that deer (j) on farm (i) has parasite at time (t)
# --> gender_ijt = gender of deer (j) on farm (i) at time (t)
# --> length_ijt = length of deer (j) on farm (i) at time (t)

# Predictors: 
# length of the host
# gender of the host. 


data("DeerEcervi")
deerData <- DeerEcervi

head(deerData)
deerData$EcerviPresence <- deerData$Ecervi
deerData$EcerviPresence[deerData$Ecervi > 0] <- 1
deerData$Gender <- factor(deerData$Sex)
# note: centering length. Otherwise, intercept represents probability that a deer of 
# length 0 has the parasite ==> nonsense since no deer has length 0. 
# Centering: gives intercept the interpretation of probability that an animal of average
# length has the parasite. 
deerData$Length.centred <- deerData$Length - mean(deerData$Length)


### GLM =================================================================================

# GLM model: Rice bird data ------------------------------------------------------------

# Assumes independence of all Y values (all richness values) including those from
# the same field. 
# Y_is = richness measured in field (i) at time (s). Assume Y_is ~ Pois(mu_is)
rice.glm <- glm(Richness ~ offset(LA) + factorSptreat + DEPTH + DEPTH2, 
                family=poisson, data=riceData)

anova(rice.glm, test="Chisq")
summary(rice.glm)
# OVERDISPERSION: inflated residual deviance (when residual deviance G >> residual df)


# GLM with quasipoisson (since the first glm indicates overdispersion)
# TODO: why is there still overdispersion?
rice.quasi.glm <- glm(Richness ~ offset(LA) + factorSptreat + DEPTH + DEPTH2, 
                      family=quasipoisson, data=riceData)
ricenull.quasi.glm <- glm(Richness ~ offset(LA), family=quasipoisson, data=riceData)
ricedep.quasi.glm <- glm(Richness ~ offset(LA) + factorSptreat + DEPTH, family=quasipoisson, 
                         data=riceData)
#anova(ricenull.quasi.glm)
summary(rice.quasi.glm)

ResidualDevianceTest(rice.quasi.glm) # model is not a good fit!
NestedLikelihoodRatioTest(ricenull.quasi.glm, rice.quasi.glm)
anova(rice.quasi.glm, test="Chisq")
NestedLikelihoodRatioTest(ricedep.quasi.glm, rice.quasi.glm)




# GLM for owl data: --------------------------------------------------------------------

with(owlData, interaction.plot(x.factor=FoodTreatment, trace.factor=SexParent, 
                               response=NCalls))
interactionPlot(x.factor="FoodTreatment", trace.factor="SexParent", 
                response="NCalls", data=owlData)
# interactionPlot(x.factor="ArrivalTime", trace.factor="SexParent", 
#                 response="NCalls", data=owlData)

# Fitting the glm owl model: 
formOwl <- formula(NCalls ~ offset(logBroodSize) + SexParent*FoodTreatment + 
                         SexParent*ArrivalTime)

owl.glm <- glm(formOwl, family=poisson, data=owlData)

anova(owl.glm, test="Chisq") # nonsignificant interaction
summary(owl.glm) # there is overdispersion since residual deviance 3641 >> residual df = 593


owl.quasi.glm <- glm(formOwl, family=quasipoisson, data=owlData)
summary(owl.quasi.glm)

# Checking interactions: the two-way interactions are not significant: 
drop1(owl.quasi.glm, test="F")

# Fitting the non-interaction model: 
owl.quasi.nointeract.glm <- glm(NCalls ~ offset(logBroodSize) + FoodTreatment + 
                                      ArrivalTime, family=quasipoisson, data=owlData)

anova(owl.quasi.nointeract.glm, test="Chisq") # significant main effects
summary(owl.quasi.nointeract.glm)


# GLM for deer data: --------------------------------------------------------------------

deer.glm <- glm(EcerviPresence ~ Length.centred * Gender, data=deerData, family=binomial)
anova(deer.glm, test="Chisq")

# Perform single-term deletions and apply likelihood ratio test.
# drop1 compares deviance of the specified model with those of nested models. 
drop1(deer.glm, test="Chi") # interaction is significant so keeping it. 

summary(deer.glm)
# PROBLEM with glm: independences: the data were obtained from 24 farms, so we are sampling
# deer that may have been in contact, so presence/absence of parasites on deer from
# the same farm cannot be assumed independent .






### GEE (Generalized Estimating Equations) ============================================


# DEFINITION GEE: 
# --> GLMM using autocorrelation in errors. 
# --> Just fixed effects, no random effects. 
# --> Similar to a random intercepts model, where for lme the random intercepts is the blocking
# variable for the GEE (id = block in GEE and random = ~1|block in random intercepts)
### GEE: id = blockVariable(i)
### LME: random = ~1|block
# --> Called a marginal model, where marginal refers to its dependence only on covariates. 
# --> GEE is useful if you have many blocks (i) and few longitudinal observations
# per blocking variable (i)


# Has Association sructure between Y_is, and Y_it, where (s) and (t) are two
# different points in time on the same group (i) from which the repeated measures
# are taken. 


# About GEE model structure: https://hyp.is/qbmz_thJEemDPO_-puJrLA/newonlinecourses.science.psu.edu/stat504/node/180/




# ASSOCIATION STRUCTURES FOR ASSOCIATION BETWEEN Y_IS, and Y_IT: 

# Let alpha_st denote correlation between observations at times (s) and (t). 
# Prerequisite: response Y must be continuous. 

# The association structure between Y_is and Y_it is represented in terms of a matrix
# where each cell (block) contains a value alpha_st representing the correlation between
# time (s) and time (t) for an individual or group or area (i)


# (1) UNSTRUCTURED CORRELATION: 

# ---> FORMULA: cor(Y_is, Y_it) = alpha_st = pearson cor             (for Y = continuous)
# is the correlation between two time obs (s), (t) from same object (i) (nest/farm...)

# --> Used when temporally sequential observations come from the same source. 
# (Example: blood pressure reading taken from same patient at regular time intervals)

# ---> Independently-estimated correlations: 
# The cells contain correlations alpha_12, alpha_13, alpha_14, .. alpha_342..
# No correlation between these parameters is assumed (estimated independently). 
# MEANING: all correlations between within-subject observations are estimated 
# independently. 
# (this incurs computational cost)




# (2) AR-1 CORRELATION (AUTOREGRESSIVE)

# ---> FORMULA: cor(Y_is, Y_it) = alpha ^ |s - t|
# is the defined correlation between two time observations (s), (t) from the same (i) block (patient, field, nest, farm)

# ----> Autoregressive correlation is observed in real-life when within-subject time 
# correlations can be modeled as "distance" of the time observations.

# ----> Used for any data when there is time / depth / age. 



# (3) EXCHANGEABLE CORRELATION: 

# ---> FORMULA: cor(Y_is, Y_it) = alpha = constant
# Meaning: this just says that the response of a batch of subjects from the same site
# are correlated. 
# Does not consider temporal or sequential correlation. 
# Ignores within-site spatial correlation. 
# Another name: "compound-symmetry correlation"
# Equivalent to RANDOM INTERCEPTS. 


# (4) STATIONARY CORRELATION: 

# Is Autoregressive up to a point then drops. 
# Example: consecutive blood readings taken every time unit; autoregressive up to a 
# time lag of 2 hours then ceases. 



#### Applicability of Each Correlation to Bird / Deer / Owl: 
# (ignoring non-continuous response, which has to be continuous for correlation strct)

# BIRD: 
### ----> each block of correlation (cell) is for a field. 
### ----> Unstructured: 
# Can use unstructured correlation matrix because observations 1,2 in field 1
# and observations 1,2 in field 2 may be temporally related. 
### ----> Autoregressive AR1: 
# Good option; correlation between fields separated by one time unit (2 weeks) is likely
# to be more similar than those separated by larger time units. 

# OWL: 
### ----> each block of correlation (matrix cell) is for a nest. 
### ----> Unstructured: 
# Assuming the arrival time of owls is regularly spaced, we can use unstructured
# correlation. Then alpha_12 represents correlation between arrivals 1 and 2 (at nest 2?), and
# alpha_13 represents correlation between arrivals 1 and 3...
### ----> Autoregressive AR1: 
# Only make sense if we cosnider the time order in the data. 

# DEER: 
### ----> each block of correlation (matrix cell) is for a farm. 
### ----> Unstructured: 
# Nonsense to use unstructured correlation because there is no relation
# between animals 1,2 at farm 1 (cell 1) and animals 1,2 at farm 2 (cell 2)
### ----> Autoregressive AR1: 
# No sense since there is no tiem order in the sampled deer per farm. 


### GEE on bird data -----------------------------------------------------------------

# GEE idea applied to bird data: 
# (1) Cannot assume that repeated measurs (richness, Y) from the same field are independent.
# (2) data are longitudinal

# COR STRUCTURE: Autoregressive since number of birds in field (i)at time (s) depends
# on those measured at time (s-1), and also less strongly at time (s - 2) and other
# times (t) in the past. 
attach(riceData)
birdDf <- data.frame(Richness[Time == 3], Richness[Time==4], Richness[Time==5], 
                     Richness[Time==6], Richness[Time==7], Richness[Time==8])
detach(riceData)
birdDf
rice.corMat <- cor(birdDf)
rice.corMat
ggcorrplot(rice.corMat)
# INTERPRET: (?) Some suggestion of correlation diminishing with increase in time lag. 

# GROUPING: given by the "id" option. The block referring to (i). The field for birds. 
# Specifies which bird observations form a block of data. 
rice.AR.gee <- geeglm(Richness ~ offset(LA) + DEPTH + DEPTH2 + factorSptreat, 
                   data=riceData, family=poisson, id=factorField, corstr="ar1")
rice.EX.gee <- geeglm(Richness ~ offset(LA) + DEPTH + DEPTH2 + factorSptreat, 
                      data=riceData, family=poisson, id=factorField, corstr="exchangeable")
rice.IND.gee <- geeglm(Richness ~ offset(LA) + DEPTH + DEPTH2 + factorSptreat, 
                      data=riceData, family=poisson, id=factorField, corstr="indep")
# note: could also use compound (exchangeable) but it is not likely that: bird numbers
# separated by 2 weeks (1 time sampling unit) have the same correlation as those 
# separated by 20 weeks (10 time sampling units)

summary(rice.AR.gee)

# INTERPRET: 
# alpha.estimate = 0.4215 = correlation between two sequential observations
# on the same field is 0.42. 
# For 1 time unit separation (lag = 1) : cor = 0.4215
# for 2 time units (obs are separated by 2 units = 4 weeks) (lag = 2) : cor = 0.4215^2
# For 3 time units (obs separated by 3 units = 6 weeks) (lag = 3) : cor = 0.4215 ^ 2

# Estimated model: 
# E[Y_it] = exp(linearpredictor...)
# var(Y_it) = 2.333 * mu_it
# cor(Y_is, Y_it) = 0.421 ^ |s - t|

# compare: glm and gee: 
# In GLM, the sptreat and depth are significant, but when GEE takes into account 
# temporal correlation, only sptreat is significant. 
summary(rice.glm)
summary(rice.quasi.glm)
summary(rice.AR.gee)


# Checking which autocorrelations tructure is best: 
ggAcf(rice.AR.gee$residuals)
ggAcf(rice.IND.gee$residuals)
ggAcf(rice.EX.gee$residuals)


### GEE on deer data -----------------------------------------------------------------

# GEE idea applied to deer data: 
# (1) Cannot assume that repeated measures from the same farm are independent. 

# COR STRUCT: exchangebale (same as random intercepts). 
# Use exchangeable correlation structure since there is 
# no specific time order between repeated measures from  the same farm. 
deer.gee <- geeglm(EcerviPresence ~ Length.centred * Gender, data=deerData, 
                   family=binomial, id=Farm, corstr="exchangeable")
deer.gee
summary(deer.gee)
# INTERPRET: 
# Interaction term: the two-way interaction Length*Gender is not significant here 
# (once the # autocorrelations are modeled) but it was in the original binomial glm.
# Dispersion parameter: phi = 1.15 , with se(phi) = 0.397 
# Not significantly different from 1: 
s <- summary(deer.gee)
1 - pchisq(s$dispersion[[1]] * deer.gee$df.residual, df=deer.gee$df.residual)
# how does lecture test it? TODO

# Cor = alpha = 0.33



# COR STRUCT: Autoregression Ar(1)
deer.ar.gee <- geeglm(EcerviPresence ~ Length.centred * Gender, data=deerData, 
                   family=binomial, id=Farm, corstr="ar1")
summary(deer.ar.gee) # alpha = 0.66

# Standard errors are larger for GEE than for GLMM, which leads to increased probability
# of Type I error. 



### GEE on owl data -----------------------------------------------------------------

# GEE idea applied to owl data: 
# (1) Cannot assume that the repeated measures from the same nest are independent.

## COR STRUCTURE: exchangebale: this means assume that all observations from the nest
# have correlation value ALPHA. 

owl.gee <- geeglm(NCalls ~ offset(logBroodSize) + SexParent * FoodTreatment + 
                        SexParent*ArrivalTime, id=Nest, family=poisson, 
                  corstr="exchangeable", data=owlData)
summary(owl.gee)

# INTERPRET: 
# alpha = 0.05 is small



# COR STRUCT: autoregressive

# Making a column to identify the group of observations from the same night and nest. 
# NestNight column tells which observations are from the same night and same nest
owlData$NestNight <- factor(ifelse(owlData$FoodTreatment == "Deprived", 
                                   paste0(owlData$Nest, ".Dep"),
                                   paste0(owlData$Nest, ".Sat")))
head(owlData)

owl.arcor.gee <- geeglm(NCalls ~ offset(logBroodSize) + SexParent * FoodTreatment + 
                              SexParent*ArrivalTime, id=NestNight, family=poisson, 
                        corstr="ar1", data=owlData)
summary(owl.arcor.gee) # bigger alpha = 0.5
summary(owl.gee)

anova(owl.arcor.gee)


# Getting the polished model: 

# Step 1: Deciding which two-way interactions are significant: 
owl.arcor.A.gee <- geeglm(NCalls ~ offset(logBroodSize) + SexParent + FoodTreatment + 
                                SexParent*ArrivalTime, data=owlData, 
                          family=poisson, id=NestNight, corstr="ar1")
owl.arcor.B.gee <- geeglm(NCalls ~ offset(logBroodSize) + SexParent * FoodTreatment + 
                                SexParent+ArrivalTime, data=owlData, 
                          family=poisson, id=NestNight, corstr="ar1")


anova(owl.arcor.A.gee, owl.arcor.gee) # the gender * food is not significant

anova(owl.arcor.B.gee, owl.arcor.gee) # the gender * arrival is not significant


# Step 2: fitting the noninteraction model
owl.arcor.nointeract.gee <- geeglm(NCalls ~ offset(logBroodSize) + SexParent + 
                                         FoodTreatment + ArrivalTime, data=owlData, 
                                   family=poisson, id=NestNight, corstr="ar1")
anova(owl.arcor.nointeract.gee) # main effects are significant. 


# Step 3: 
# Fitting the gender last to see if significant: ?
anova(geeglm(NCalls ~ offset(logBroodSize) + FoodTreatment + ArrivalTime + SexParent, data=owlData, 
             family=poisson, id=NestNight, corstr="ar1"))
# Not significant so fit model without gender

# Step 4: fit the final model
owl.arcor.final.gee <- geeglm(NCalls ~ offset(logBroodSize) + FoodTreatment + ArrivalTime, 
                              data=owlData, family=poisson, id=NestNight, corstr="ar1")
anova(owl.arcor.final.gee)
summary(owl.arcor.final.gee)

# INTERPRET: 
# alpha+st = 0.517 ==> correlation of the calls between two sequential arrivals is 0.51
# which is quite high. 
# Oversipersion = 6.6, similar to quasipoisson glm. 
# p-values are larger than for quasi-poisson glm. 

# CONCLUSION: there is a foodtreatment effect (lower number of calls from food-satiated
# observations) and ArrvalTime: the later the night, the fewer calls (due to negative slope)
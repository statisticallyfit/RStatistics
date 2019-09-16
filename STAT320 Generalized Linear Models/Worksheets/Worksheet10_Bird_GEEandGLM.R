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
library(ggcorrplot) # for ggcorrplot
library(forecast) # for ggAcf

options(show.signif.stars = FALSE)




data("RiceFieldBirds")

riceData <- RiceFieldBirds
head(riceData)

# This calculates species richness: number of species not number of birds. 
riceData$Richness <- rowSums(riceData[, 8:56] > 0)
riceData$factorField <- factor(riceData$FIELD)
riceData$LA <- log(riceData$AREA) # offset(LA) allows for sites of different size (AREA)
riceData$factorSptreat <- factor(riceData$SPTREAT)
riceData$DEPTH2 <- riceData$DEPTH ^2 


# Correlaion among repeated measures: 
attach(riceData)
birdDf <- data.frame(Richness[Time == 3], Richness[Time==4], Richness[Time==5], 
                     Richness[Time==6], Richness[Time==7], Richness[Time==8])
detach(riceData)
birdDf

birdCorMat <- cor(birdDf)
birdCorMat
ggcorrplot(birdCorMat)
# INERPRET: some suggestsion of correlation diminishing with an increase in 
# time lag (???why, actually seems to increase with increase in time lag.)


# GOAL of study: explain the richness values as a function of dpeth and management
# fefects. 
# Response = count (poisson glm)
##### OFFSET: need offset because original data were dnesities; numbers per field, and
# the sizes of the fields are different, so log of size of field is offset variable. 
# X1 = depth (quadratic by past experience)
# X2 = management



# xyplot of species richness plotted against time (expressed in two-weekly periods).
# Each panel represents a different field. 
xyplot(Richness ~ Time | factorField, data=riceData, panel = function(x,y){
      panel.grid(h = -1, v=2)
      panel.points(x,y)
      panel.loess(x, y, lwd=2)
})

# INTERPRET: 
# There are 11 fields in this site, and each
# field was repeatedly sampled; see Fig. 12.1. Note that there is a general decline in
# bird numbers over time. One of the available covariates is water depth per field, but
#water depth and time are collinear (as can be inferred from making an xyplot of
# depth versus time for each field), so we avoid using them together as covariates in
# the models.


# GLM model: Rice bird data ------------------------------------------------------------

# Assumes independence of all Y values (all richness values) including those from
# the same field. 
# Y_is = richness measured in field (i) at time (s). Assume Y_is ~ Pois(mu_is)
rice.glm <- glm(Richness ~ offset(LA) + factorSptreat + DEPTH + DEPTH2, 
                family=poisson, data=riceData)


# GLM with quasipoisson (since the first glm indicates overdispersion)
# TODO: why is there still overdispersion?
rice.quasi.glm <- glm(Richness ~ offset(LA) + factorSptreat + DEPTH + DEPTH2, 
                      family=quasipoisson, data=riceData)
summary(rice.quasi.glm)

# Residuals plots
autoplot(rice.quasi.glm, which=c(1,2,3,6))
# residuals are normal apart from a few outliers

# Autocorrelation plot
ggAcf(rice.quasi.glm$residuals)
# PROBLEM: sinusioidal pattern and some acfs are past the 2 std error lines
# So suggests some correlation left nonrandom for first few lags 1-10



# GEE model : Rice bird data ------------------------------------------------------------


# COR STRUCTURE: Autoregressive since number of birds in field (i)at time (s) depends
# on those measured at time (s-1), and also less strongly at time (s - 2) and other
# times (t) in the past. 
# GROUPING: given by the "id" option. The block referring to (i). The field for birds. 
# Specifies which bird observations form a block of data. 
rice.gee <- geeglm(Richness ~ offset(LA) + DEPTH + DEPTH2 + factorSptreat, 
                   data=riceData, family=poisson, id=factorField, corstr="ar1")

summary(rice.gee)
# INTERPRET and compare with GLM: 
# estimates change and stderrors INCREASE and p-values INCREASE but interpretation
# is the same. All terms are still significant. 

# Correlation between two sequential (lag=1) observations in same field: 
# alpha_st = 0.422

# Correlation between observations with time lag of two units (4 weeks): then |s-t| = 2
alpha_st = 0.422
alpha_st ^ 2

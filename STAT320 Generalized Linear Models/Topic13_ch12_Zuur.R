setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
#library(lme4)
#detach(package:lme4)
library(lattice)



# CALIFORNIA BIRD DATA: 
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
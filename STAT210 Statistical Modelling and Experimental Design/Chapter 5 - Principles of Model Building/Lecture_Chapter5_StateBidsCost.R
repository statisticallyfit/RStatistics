setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 5 - Principles of Model Building/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')

library(ggplot2)

bidsData <- read.table("bids.txt", header=TRUE)
bidsData$State <- factor(bidsData$State)
bids1.lm <- lm(Cost ~ State, data=bidsData)
summary(bids1.lm)


# Baseline = Kansas

# Global F-test is significant: says that model is useful. Cost between
# states are different between the three states. The mean cost is different
# for at least one of the states. 

# Kentuchy intercept: mean cost for Kentuchy is higher than for Kansas,
# but the difference is not statistically significant. 

# Texas state: mean cost for Texas is higher than for Kansas, AND the 
# difference is also stat. sig. Mean cost for Texas is sig. higher than
# mean cost for Kansas. 

# R^2 is horrible - need more predictor variables. 


# Now make Texas as base
bidsData.Texas <- bidsData
bidsData.Texas$State <- relevel(bidsData$State, ref="Texas")
bids.texas.lm <- lm(Cost ~ State, data=bidsData.Texas)
summary(bids.texas.lm)

# Kentucky coef: mean cost for kentucky is lower than for Texas and
# the difference is not significant. 

# Kansas coef: mean cost for Kansas is lower than for Texas and the
# difference is significant. 

# Intercept: mean cost for Texas is positive and significant. 



### VERIFYING THE ESTIMATED MEAN ANNUAL COST FOR EACH STATE: 
summary(bids1.lm)
muKansas = bids1.lm$coefficients[[1]]; muKansas
muKentucky = bids1.lm$coefficients[[2]] + muKansas; muKentucky
muTexas = bids1.lm$coefficients[[3]] + muKansas; muTexas



## CONFIDENCE INTERVALS
betaCI(bids1.lm)

# intercept CI = mean of kansas is between 170 and 389

# kentucky coef CI = mean diff between kentucky and kansas = (0 is a reasonable
# value between the difference of their means, consistent with insignificant pval)

# texas CI = mean diff between texas and kansas is between 43 and 353 with 95% conf. 



# TO GET INDIVIDUAL CONF INTERVALS - remove intercept
# (removing intercept won't work for more than 1 predictor dummy type)
bids.nointercept.lm <- lm(Cost ~ State -1, data=bidsData.Texas)
betaCI(bids.nointercept.lm)

# Same means from tapply as from betaCI (fits)
with(bidsData.Texas, tapply(Cost, list(State), mean))
with(bidsData, tapply(Cost, list(State), mean))
     
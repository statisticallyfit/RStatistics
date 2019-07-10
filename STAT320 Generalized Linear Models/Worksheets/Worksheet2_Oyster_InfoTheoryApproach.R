setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/Worksheets/")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')


library(ggplot2)
library(ggeffects)
library(lattice)
library(lindia)

oysterData <- read.table("oyster.txt", header=TRUE)

oysterData$watertemp <- factor(oysterData$watertemp, labels=c("low", "medium", "high"))
#attach(oysterData)
#oysterData$watertemp[watertemp == 1] <- "low"
#oysterData$watertemp[watertemp == 2] <- "medium"
#oysterData$watertemp[watertemp == 3] <- "high"
#detach(oysterData)

# The data set: 
# waer temperature, initial weight of ten oysters, final weigh of ten oysters, one month later

# par a) ------------------------------------------------------------------------

# how to fix the incorrect observation: 
# -> retake the analysis
# -> remove the observation from the data sets
# -> double check the data entry at time of observaions
# -> simple decimal error, easy to fix, just move the decimal

oysterData$final[12] <- 30.2
oysterData


# part b) ----------------------------------------------------------------------------

# Plot data using xyplot to compare the response variable (final weight) across
# the three categories of water temperature. 

oyster.xyplot <- xyplot(final ~ initial | watertemp, data=oysterData, 
                        layout=c(3,1), # using layout = c(3,1) to produce 3 plots side by side
                        panel=function(x, y) {
                              panel.xyplot(x, y)
                              panel.lmline(x, y)
                        } # end of panel function 
) # end of xyplot function 

oyster.xyplot

### intercepts: only slightly different for the three factor levels
### slope: nearly the same for final weight, across all factor levels of watertemp => 
# this suggests interaction model NOT necessary


# part c) global model ---------------------------------------------------------------

# watertemp = factor with 3 levels so 2 dummy variables are needed
#     watertempMED = {1 if temp = MEDIUM, 0 if not
#     watertempHIGH = {1 if temp = HIGH, 0 if not
#     base level = low

# E(Y) = B0 + B1*initial + B2*watertempMEDIUM + 
#           B3 * watertempHIGH + 
#           B4 * watertempMEDIUM * initial + 
#           B5 * watertempHIGH * initial

# watertempLOW model: (watertempMED, HIGH = 0)
# E(Y) = B0 + B1*initial 

# watertempMEDIUM model: (watertempMEDIUM = 1, watertempHIGH = 0)
# E(Y) = B0 + B1*initial + B2 + B4 * initial
# E(Y) = (B0 + B2) + (B1 + B4) * initial

# watertempHIGH model: (watertempHIGH = 1, watertempMEDIUM = 0)
# E(Y) = B0 + B1*initial + B3 + B5*initial
# E(Y) = (B0 + B3) + (B1 + B5)*initial


# ---> B0 = intercept for the watertempLOW model
# ---> B1 = slope for watertempLOW model
# ---> B2 = difference in intercepts between watertempMEDIUM and watertempLOW
# ---> B3 = difference in intercepts between watertempHIGH and watertempLOW
# ---> B4 = difference in slopes between watertempMEDIUM and watertempLOW
# ---> B5 = difference in slopes between watertempHIGH and watertempLOW


# part d) ------------------------------------------------------------------------------
# GOAL: using Info-Theory approach to model selection, find the best model to describe
# the effect of the predictors on the final weight of oysters. 

library(AICcmodavg)

candidateModels <- list()
candidateModels[[1]] <- lm(final ~ initial + watertemp + initial:watertemp, data=oysterData)
candidateModels[[2]] <- lm(final ~ initial + watertemp, data=oysterData)
candidateModels[[3]] <- lm(final ~ initial, data=oysterData)

modelNames <- c("interaction", "temp_initial", "initial")

aicResults <- aictab(cand.set = candidateModels, modnames = modelNames, sort=TRUE)
aicResults
# so the interaction model is second best, and the initial-only model is not even considerable
# Preferred model is temp + initial model since delta_AICc = 0, so it has the minimum
# AICc value. 


# THE NHST approach (non info theory approach): ------------
oyster.interact.lm <- candidateModels[[1]]
oyster.both.lm <- candidateModels[[2]]
oyster.initial.lm <- candidateModels[[3]]

anova(oyster.interact.lm, oyster.both.lm, oyster.initial.lm)
# p-value on line 2 = 0.2907 => means that the interaction term not significant
# p-value on line 3 is 7.9e-5 => means the only different term (watertemp) is significant

# CONCLUSION:  the "both" model of initial + watertemp seems better than interaction model
# even using the NHST approach 


# Gives more detail about each coefficient level but offers no comparison of models, like anova
# summary(oyster.interact.lm)
# summary(oyster.both.lm)
# summary(oyster.initial.lm)


# part e) -------------------------------------------------------------------------------
# Confidence intervals for regression coeffs and interpretations

options(digits=2)

# MODEL:  
# E(Y) = B0 + B1 * initial + B2*watertempMEDIUM + B3 * watertempHIGH 

# line for temp = medium: 
# E(Y) = B0 + B1 * initial + B2 
#      = (B0 + B2) + B1*initial

# line for temp = high: 
# E(Y) = B0 + B1 * initial + b3
#      = (B0 + B3) + B1 * initial 

# --> B0 = (?)
# --> B1 = the common slope of initial weight, for all levels of water temp
# --> B2 = difference in tercepts between  watertemp=MEDIUM and LOW
# --> B3 = difference in intercepts between watertemp=HIGH and LOW

betaCI(oyster.both.lm)

# --> initial coef: we are 95% confidence that for a one unit increase in 
# initial weight (for all watertemp levels) 
# the final weight increases between 0.98, 1.2 units.

# --> watertempmedium COEF: we are 95% confidence that the difference in intercepts 
# between the medium and low water temperature models is between 0.97 and 2.3, so the final 
# weight is larger for medium than low temperature. 
# This difference is statistically significant because the entire CI is above 0. 

# ---> watertemphigh COEF: we are 95% confident that the difference intercepts between
# the high and low water temperature models is between -0.84 and 1.2. 
# This difference is NOT significant since the CI contains 0. 


# SIMPLY: there is a significant diff in inercepts between MED and lOW lines, but
# not between the HIGH and LOW lines. 


### Resetting the baseline reference group to level (medium) allows us to compare
# intercepts between levels MEDIUM and HIGH as well. 
oysterData.baseMEDIUM <- oysterData
oysterData.baseMEDIUM$watertemp <- relevel(oysterData$watertemp, ref="medium")
oysterData.baseMEDIUM$watertemp
# now we see this releveling worked because MEDIUM is the first item in the list of levels

# Refit the model
oyster.both.baseMEDIUM.lm <- lm(final ~ initial + watertemp, data=oysterData.baseMEDIUM)

betaCI(oyster.both.baseMEDIUM.lm)

# --> watertemplow: diff in intercepts of the LOW - MED lines: difference is negative
# since final temp for LOW level is lower than for MEDIUM temp, and the difference is 
# significant since the CI is all below 0 (does not contain 0)

# ---> watertemphigh: difff in intercepts of the lOW - HIGH lines: difference is negative
# since final temp for low level is lower than for HIGH temp, and the difference is 
# significant since the CI is all below 0 (does not contain 0)


### CHECK MODEL ASSUMPTIONS: 
autoplot(oyster.both.lm, which=c(1,2,3,4))
shapiro.test(oyster.both.lm$residuals) # large p-value, no evidence of non-normal resids

# NORMALITY OF ERRORS: seems satisfied, since residuals vs fitted plot contains no 
# curvature and normal qq plot is nearly linear, with no strong deviation from the line. 
# HOMOSKEDASTICITY OF ERRORS: seems satisfied since no fanning shape in res vs fit plot. 

# ===> model assumption e ~ N(0, sigma^2) appears valid


# CHECK OUTLIERS / INFLUENTIAL POINTS: 
p <- 4 # num predictors
n <- nrow(oysterData)

# NOTE: this particular plot does not say if the points are influential. 
gg_cooksd(oyster.both.lm) # so there are 2 influential points, observations 10 and 19

cook.df <- influence.cooksDistances(oyster.both.lm)
cook.df

# From the plot, observation 19 and 10 cross the line but are only influential if they
# pass 50% in the F-distribution. 
cook.df$CooksPoints[19] # the cooks distance of the 19th observation
cook.df$CooksPoints[10] # the cooks distance of the 10th observation

# Check which quartile they are at in the F distribution
pf(cook.df$CooksPoints[19], df1=p, df2=n-p-1) # so at 16 percentile
pf(cook.df$CooksPoints[10], df1=p, df2=n-p-1) # so at 7th percentile


### Check the LEVEERAGE VALUES
influence.leverageValues(oyster.both.lm)
# observation 19 is influential using the leverage values



# part f) -----------------------------------------------------------------------------

# State the equation of the line, for each temperature level: 

#anova(oyster.interact.lm)


# MODEL:  
# E(Y) = B0 + B1 * initial + B2*watertempMEDIUM + B3 * watertempHIGH 

# line for temp = low
# E(Y) = B0 + B1 * initial 

# line for temp = medium: 
# E(Y) = B0 + B1 * initial + B2 
#      = (B0 + B2) + B1*initial

# line for temp = high: 
# E(Y) = B0 + B1 * initial + b3
#      = (B0 + B3) + B1 * initial 

# --> B0 = (?)
# --> B1 = the common slope of initial weight, for all levels of water temp
# --> B2 = difference in tercepts between  watertemp=MEDIUM and LOW
# --> B3 = difference in intercepts between watertemp=HIGH and LOW

summary(oyster.both.lm)

### calculated line for temp = low:  
# E(Y) = 2.56 + 1.0716*initial

### calculated line for temp = medium: 
# E(Y) = (2.56 + 1.6096) + 1.0716*initial
#      = 4.2 + 1.0716*initial

### calculated line for temp = high: 
# E(Y) = (2.56 + 0.174) + 1.0716 * initial
#      = 2.7 + 1.0716*initial


### Update the model to get estimates for each intercept and slope but not 
# the differences between intercepts
oyster.nointercept.lm <- update(oyster.both.lm, . ~ . -1)
summary(oyster.nointercept.lm) # the exact intercepts as above


# part g) -------------------------------------------------------------------------------

# estimate the final mean weight of oysters with initialweight = 20, at all temperature
# levels. 
options(digits=5)

newX <- expand.grid(initial=20, watertemp=1:3)
newX
newX$watertemp <- factor(newX$watertemp, labels=c("low", "medium", "high")) 
newX
preds <- predict(oyster.both.lm, newdata=newX, interval="confidence")
preds
result <- cbind(newX, preds); result


# part h) -------------------------------------------------------------------------------

# SUMMARY: 

# intercepts: the mean final weight is significantly different for oysters grown in 
# low VS. medium and  medium VS. high water temperatures. 

# slope: the increase in mean final weight of oysters as initial weight increases is
# the same at all water temperature levels. 

# Prediction: for a given initial weight, the predicetd mean final weight is significantly
# greater for oysters grown in the medium water temperature. 

      # --> Medium water temperature: CI is non-overlapping with any other and is above
      # all other CI's so: predicted mean final weight is significantly higher for
      # oysters grown in medium water temperature. 
      
      # --> Final weights not different for oysters grown in low vs high temperature waters. 
setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
#library(lme4)
#detach(package:lme4)
library(lattice)

options(show.signif.stars = FALSE)


# CONCEPTS REVIEW: ---------------------------------------------------------------------

# SAME IDEA AS IN TOPIC 10 (HIERARCHICAL MODELS): 

# Idea: grouped data. Each  random effect consists of a group of 
# repeated measures. 

# There are two levels of variability:
# (1) measurement error on each individual within a group. 
# (2) variability amongst groups


# Data:  --------------------------------------------------------------------------------
# Replications = 5 (5 weekly measures on each rat)
# N = 27 rats
# Y = body weight of rat
# Treatment = treat (levels: control, or thiouracil or thyroxine)
# Grouping factor or Unit: subject (the rat)
# Time: weeks
# 
# (10 rats are in control, 7 rats are given thyroxine in water, and 10 rats are given water
# with thiouracil added)



drinkData <- read.table("data/rd.txt", header=TRUE)
head(drinkData)

N <- nrow(drinkData)
drinkData$subject <- factor(paste(rep('R', N), drinkData$subject, sep=''))
drinkData$Weeks <- factor(paste(rep('W', N), drinkData$weeks, sep=''))
drinkData


# part (a) -----------------------------------------------------------------------------

# Exploratory plots for relation between weight/time for different treatments

# Unit = rat, the subject (grouping factor)
# Time (primary covariate) (NOTE: must be NUMERICAL, never quantitative)
# Treatment: treat (thyroxine ...)
### outer = treat
### Random part: Unit x Time = ~Time | subject part (random effects from how units react over time)
### Fixed part: Treatment x Time = treat * Time


# PLOT 1: 
# usable for identifying (1) between-group variation, and (2) within-group variation

# formula = response ~ primaryCovariate | groupingFactor or ~1 |groupingFactor when 
# no other suitable candidate
drinkData <- groupedData(wt ~ weeks | subject, outer= ~treat, data=drinkData)
plot(drinkData, outer=TRUE)

# Or by ggplot: 

numSubjects <- length(levels(drinkData$subject))
ggplot(data=drinkData, aes(x=weeks, y=wt, color=subject)) + geom_line() + geom_point() +
      facet_wrap(~ treat, ncol=3) + 
      scale_colour_manual(values=ggplotColors(numSubjects))
      #scale_colour_manual(values=sample(ggplotColors(numSubjects), size=numSubjects))
      #scale_colour_manual(values=sample(rainbow(numSubjects), size=numSubjects))
      

# INTERPRET:  to see structure of ssytematic and random effects, we are plotting profiles 
# for each experimental unit (rat) across time (weeks). 

# (i) linearity in response: response is slightly concave up for control and
# thyroxine group but concave down for thiourcaile group so this group had
# less weight increaseas time went on, compared to others. 

######## (1) Addressing: VARIABILITY AMONGST GROUPS: (fixed effect interaction)

### --- Intercepts among treatments:
# (ii) similar intercepts for all treatment groups. 

### --- Slopes among treatments:
# (III) growth rate (slope) seems steeper for control and thyroxine as time goes on
# compared to thiouracil. Increasing weight gain as time goes on treats 1,3


######## (2) Addressing: 
#   VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# where is sigma.residual (???)

# (IV) Random Intercepts: (Space between lines within treat): 
# Within each treatment group there is variability among subjects since the 
# lines are spaced out. True for control since lines are spaced out, true for
# thiouracile since lines are spaced out ... etc 
# =====> suggests some need for random intercepts model: ~|subject

# (V) Random slopes: (Slopes within treat): the rate of increase in weight over time
# (slope) is similar for all subjects/rats within a particular treatment group. 
# So for thiouracil, see that lines all have similar slope. 
# For control, all lines have similar concave up shape. 
# For thyroxine, half the lines have steeper slope than other half. 
# ====> suggests some need for random slopes: ~Time|subject


# (vi) Fanning out: differences amongst rats at the beginning are preserved throughout
# the trial  (since the lines are spaced similarly over time). True for Control and
# Thiouracil but not for Thyroxine. 

## (vii) RESIDUAL Variation: ??? 
# This is the overall within-group (within-subject) variation
# Within each treatment group there is variability among subjects since the 
# lines are spaced out. 
# Boxplot reflects this as wider boxplots for all treatment groups. 





# PLOT 2: 
# usable for identifying (1) between-subject variation, and (2) within-subject variation

### INTERCEPTS AND SLOPES 95% CI: 

#### Can visualize the mean response as a weighted average of the individual rat profiles
# provided that the between-rats variance is included in the averaging. 
# Finding linear response for each rat using lmList()
# Can help explain random effects when the model is fitted. 
drink.lmlist <- lmList(wt ~ weeks | subject, data=drinkData)
drink.lmlist

# note: for this to work, do NOT load library(lme4) and nlme at the same time
drink.ci <- intervals(drink.lmlist) 
plot(drink.ci)

#INTERPRET: 
# Intercepts: the intercepts vary BETWEEN subjects so suggests random intercepts model
# Slopes = lots of variation so suggests random slopes model

## should fit: randintercepts/random slopes model


# Another way to plot using ggplot: 
intDf <- data.frame(drink.ci[,,1]) # drink.ci[,,"(Intercept)"])
slopeDf <- data.frame(drink.ci[,,2]) # drink.ci[,,"weeks"])
subjectNames <- rownames(intDf) # same for int and slope dfs

ptreat = drinkData$treat[match(levels(drinkData$subject), drinkData$subject)]

ciDf <- data.frame(Estimate=c(intDf$est., slopeDf$est.), 
      Lower = c(intDf$lower, slopeDf$lower),
      Upper = c(intDf$upper, slopeDf$upper),
      Type=c(rep("intercept", nrow(intDf)), rep("slope", nrow(slopeDf))),
      subject= subjectNames, 
      treat = ptreat)

ggplot(ciDf, aes(x=Estimate, y=subject, color=treat)) + 
      geom_errorbarh(aes(xmin=Lower, xmax=Upper)) + 
      geom_line() + geom_point() + facet_wrap(~Type, ncol=2) 



# ANother way to draw the boxplots all-in-one approach (faraway sheet)# 
# (averaging over the subjects)

#ptreat = drinkData$treat[match(levels(drinkData$subject), drinkData$subject)]
#boxplot(split(slopeDf$est., ptreat))
#boxplot(split(intDf$est., ptreat))

library(reshape2)

slopeByTreatDf <- melt(split(slopeDf$est., ptreat))
colnames(slopeByTreatDf) <- c("slope", "treat")
ggplot(slopeByTreatDf, aes(x=treat, y=slope, color=treat)) + geom_boxplot(size=1)

# (1) VARIABILITY AMONGST GROUPS (fixed effect interaction)

### --- Slopes among treatments:
# For lines plot, the rowth rate (slope) seems steeper for control and thyroxine
# compared to thiouracil. On the boxplot, we can see this steeper slope by seeing that
# boxplots for control and thyroxine are much higher than for thiouracile. 

# (2) VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# ----- Random slopes: (Slopes within treat): the rate of increase in weight over time
# (slope) differs for subjects within a treatment (can see this more clearly than on lines plot)
# For thiouracil, there is least variation in slopes among subjects compared to other treats.
# For control, has next-largest variation since boxplot is next-narrowest. 
# For thyroxine, half the lines have steeper slope than other half (in lines plot) and 
# boxplot reflects this since the length of the boxplot is thickest compared to other
# groups. 

# ====> suggests some need for random slopes: ~Time|subject


intByTreatDf <- melt(split(intDf$est., ptreat))
colnames(intByTreatDf) <- c("intercept", "treat")
ggplot(intByTreatDf, aes(x=treat, y=intercept, color=treat)) + geom_boxplot(size=1)

# (1) VARIABILITY AMONGST GROUPS (fixed effect interaction)
### --- Intercepts among treatments:
# For lines plot, there is similar intercepts for all treatment groups. On boxplot, 
# we see this as: the boxplots are overlapping , so are similar in median value. 


# (2) VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# ----- Random Intercepts: (Space between lines within treat): 
# Within each treatment group there is variability among subjects since the 
# lines are spaced out. 
# Boxplot reflects this as wider boxplots for all treatment groups. 
# For control, the boxplot is wide, so lots of variation of intercepts among subjects within
# the control group. 
# Same for thiouracil and thyroxine. 

# =====> suggests some need for random intercepts model: ~ 1|subject





## PLOT 3
# usable for identifying (1) between-group(treat) variation (so just fixed effect interaction)

# Interaction plot of week and treatment: 
with(drinkData, interaction.plot(x.factor=weeks, trace.factor=treat, response=wt))

interactionPlot(data=drinkData, x.factor="weeks", trace.factor="treat", response="wt")

# INTERPRET: suggests interaction between treat and week starting from week 2 onwards
# and the thiouracile value. 

# INTERPRET: (sol)
# Shows trend over time for the three treatment groups, averaged over subjects. 
# Rate of increase in weight over time is similar for the two treatment groups
# but is lower for the rats given thiouracile since thiouracil slope is lower. 

# ===> suggests fixed effect interaction: Time * Treat



# STRATEGY: plots 1, 2, 3 suggests a fixed-effects interaction model + random effects
# model using random slopes and intercepts. 

# NOTE: no need to centre weeks since weeks = 0 is a meaningful measure. 



# part (c) ---------------------------------------------------------------------------

# Fitting the fixed effects interaction + randomslopes/intercepts model: 
drink.lme <- lme(wt ~ treat*weeks, random= ~weeks|subject, data=drinkData)
# Fitting fixed effects interaction + random intercepts (only)
drink.intercept.lme <- lme(wt ~ treat*weeks, random = ~1|subject, data=drinkData)

# Comparing the two: 
anova(drink.intercept.lme, drink.lme)
# p-value is small ===> the random slopes term is significant. 
# (OR) if going by AIC, say AIC for random slopes/ints is much less than for intercepts-
# only model. 


#### ANALYZING fixed-effects part of RandomSlopes/random intercepts model: --------------

# So getting the output for the random slopes/random intercepts model: 
anova(drink.lme) # shows only fixed effects (between group variation)

# INTERPRET: 
# treat:weeks term has low p-value 
# ===> interaction between treat and weeks is significant 
# ===> change in weight over time (slope) is NOT the same for all treatment groups 
# ===> keep the fixed-effects interaction term. 

summary(drink.lme)

# INTERPRET: 
### --> treatThiouracile:Weeks coeff is significant ==>  
# ===> change in weight over time IS significantly different for (throucail vs control)
### Confirms interaction plot: slopes are not different for thyroxine and control

### --> treatThyroxine:Weeks coeff NOT significant ==> change in weight over time is NOT
# significantly different for the (thyroxine vs control)
### Confirms interaction plot: slopes ARE different for thiouracil and control, which
# thiouracil being LOWER than control. 


### --> treatThiouracil coeff is not significant (can interpret this main effect
# since its corresponding thiouracil:weeks interaction term IS significant)
# Note that treatThiouracile main effect is not lower than control (averaged over weeks)
# but the treatThiouracil:weeks is lower than control, when taking into account weeks.


#### ANALYZING random effects part of RandomSlopes/random intercepts model: --------------

VarCorr(drink.lme)
# var.intercept = 32.49
# var.weeks = 14.138
# var.residual = 18.905       (overall within-subject variation)

# INTERPRET: intercept is much larger than var.weeks (slopes) so this might suggest
# just a random intercepts model. Checking significance for sure: 


# Significance of variance components: 
drink.var.ci <- intervals(drink.lme)
d = drink.var.ci$reStruct$subject
drink.var.ci

# INTERPRET: 
# (1) sigma.weeks(slope) and sigma.intercept are overlapping so not statistically different
# from each other

# (2) all Ci are above zero (sigma..resid, sigma.int, sigma.slope) so the variance 
# components are all significantly different from zero. ===> so random slopes / and
# random intercepts model is needed? 

# (3) sigma.intercept and sigma.resid overlap
# (4) sigma.slope and sigma.residual overlap







# part (e) Residuals plots ----------------------------------------------------------


fixedR = drink.lme$residuals[,1]
df <- data.frame(RandResids=drink.lme$residuals[,2], 
                 RandStdResids=resid(drink.lme, type="normalized"), 
                 FixedResids=drink.lme$residuals[,1],
                 FixedStdResids = (fixedR - mean(fixedR))/sd(fixedR), 
                 RandFits=drink.lme$fitted[,2], 
                 FixedFits = drink.lme$fitted[,1],
                 Rand = drinkData$subject, 
                 Fixed = drinkData$treat)

# Residuals vs fitted -----------------------------------------------------------------

# (1) Residuals vs fitted (by Rat (random part))
ggplot(data=df, aes(x=RandFits, y=RandStdResids, color=Rand)) + geom_point(size=2) + 
   geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
   geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
   ggtitle("Residuals vs Fitted for Unit (Grouping Factor = subject)")

# This is an interaction model so we plot residuals by variable 1 (Gender)
# Residuals vs fitted (by Gender)
plot(drink.lme, resid(., type="p") ~ fitted(.) | subject, abline=0)


# (2) Residuals vs fitted (by Age (systematic or fixed part))

ggplot(data=df, aes(x=FixedFits, y=FixedStdResids, color=Fixed)) + geom_point(size=2) + 
   geom_hline(yintercept=0, linetype="dashed", size=1,color="red") + 
   facet_grid(. ~Fixed) + 
   #facet_wrap( ~ageC, ncol=2) + 
   ggtitle("Residuals vs Fitted for Fixed Line")



### NOTE: the lecture notes does the Random Resids (Rat) but colord/partitioned by Diet: 

plot(drink.lme, resid(., type="p") ~ fitted(.) | treat, abline=0, layout=c(3,1))

ggplot(data=df, aes(x=RandFits, y=RandStdResids, color=Fixed)) + geom_point(size=2) + 
   geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
   geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
   #facet_grid(. ~ageC) + 
   facet_wrap( ~Fixed, ncol=3) + 
   ggtitle("Residuals vs Fitted")


### QQ plot of residuals (fitted) ----------------------------------------------------

shapiro.test(df$FixedResids) # for treat
shapiro.test(df$RandResids) # for subject, no evidence to reject normality

# (1) Standardized residuals by treat (Fixed part): 
qqnorm(drink.lme$residuals[,1])

ggplot(df, aes(sample = FixedStdResids)) + 
   stat_qq(color="dodgerblue", size=3, alpha=0.5) + 
   stat_qq_line(linetype="dashed", size=1) + 
   ggtitle("QQnorm plot for Fixed Line Standardized Residuals")

# (2) Standardized residuals by Subject (random part): 
qqnorm(residuals(drink.lme, type="normalized"))

ggplot(df, aes(sample = RandStdResids)) + 
   stat_qq(color="dodgerblue", size=2) + 
   stat_qq_line(linetype="dashed", size=1) + 
   ggtitle("QQnorm plot for Random Effect Standardized Residuals")



### Residuals vs predictors (using boxplots) -------------------------------------------

# (1)  Boxplot of residual vs predictor (by treat (fixed part))
ggplot(df, aes(x=Fixed, y=FixedStdResids, colour=Fixed)) + geom_boxplot(size=1) + 
   geom_hline(yintercept=0, linetype="dashed",color="black",size=1)
# homogeneity of variance and all centred around mean = 0
# but some outliers

# (2) Boxplot of residual vs predictor (by subject (random part))

plot(drink.lme, subject ~ resid(., type="p"),abline=c(0,1))

## CORRECTED: color by Fixed var (treat) not by Rand (subject), which is already on x-axis!
ggplot(df, aes(x=Rand, y=RandStdResids, colour=Fixed)) + geom_boxplot(size=1) + 
   geom_hline(yintercept=0, linetype="dashed", size=1,color="black")

# NO homogeneity of variance. 


# Residuals plots above do not suggest any problems.
# Residuals randomly scattered around 0 and only one with a value greater than
# 2 std residuals.



## Predictions
preds <- predict(drink.lme, level=0:1)
preds

with(drinkData, cbind(weeks, treat, preds))

# The fifth column gives the predicted values for the fixed component
# only. Note that the predicted value for the fixed component is the
# same for each rat from the same treatment for the same week.
# So, for example, the predicted mean weight in week 0 for all of the
# rats in the control treatment (subjects 1 – 10) is 52.88. That is, the
# fixed component is averaged over the rats.
# The sixth column gives predictions for each subject (rat).

random.effects(drink.lme)

# predict.subject: Rat = R27, treat = thiouracil, week = 3
# fixed part: (under predict.fixed)
f27 = 108.99000
# random part line: (foudn from random effects): rand = -2.8041634 -0.3674693*weeks
numWeeks = 3
r27 = -2.8041634 + -0.3674693 * numWeeks
r27

# total line (under predict.subject)
f27 + r27
# 105.083





# part (f) --------------------------------------------------------------------------

# AUTOCORRELATIONS 

# ACF  for all residuals combined
drinkResids <- drink.lme$residuals[,2]
acf(drinkResids) # residulas for random part only

# or by ggplot
library(forecast)
ggAcf(drinkResids)

# INTERPRET: 
# ACF does not appear to have a random distribution 
# Some autocorrelations are greater than 2 stdevs (dotted lines)
# ===> suggests a systematic effect may be missing. 


# ACF for residuals by subject
rat.level <- levels(drinkData$subject)

# Getting default plot values
# par("mfrow", "oma", "mar")
# $mfrow
# [1] 1 1
# $oma
# [1] 0 0 0 0
# $mar
# [1] 5.1 4.1 4.1 2.1
par(mfrow=c(3,3), mar=c(2,4,0,0), oma=c(2,2,4,2))

for(i in seq(along=rat.level)){
   datain <- drinkData$subject == rat.level[i]
   
   acf(drinkResids[datain], xlab="", ylab="", main=paste("rat", rat.level[i]))
   #print(ggAcf(drinkResids[datain])) + ggtitle(paste("rat", rat.level[i]))
}

par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(5,4,4,2)+0.1)





## Supplementary Question 1: -----------------------------------------------------

# Getting the fixed effect lines for the three treatment groups
summary(drink.lme)

# Full Line: 
# A = Thiouracil
# C = Control
# X = Thyroxine

# Y = 52.88 + 4.78*(A) - 0.79429*(X) + 26.48*(weeks) - 9.37*(A:weeks) + 0.66286*(X:weeks)

# Control: X = 0, A = 0
# Y = 52.88 + 26.48*(weeks)

# Thyroxine (X): X = 1, A = 0
# Y = 52.88 -  0.79429*(1) + 26.48*(weeks) + 0.66286*(weeks)
#   = (52.88 - 0.79429) + (26.48 + 0.66286)*weeks
#   = 52.08571 + 27.14286*weeks

# Thiouracil (A): A = 1, X = 0
# Y =  52.88 + 4.78*(1) + 26.48*(weeks) - 9.37*(weeks) 
#   = (52.88 + 4.78) + (26.48 - 9.37)*weeks
#   = 57.66 + 17.11*weeks



## Supplementary Question 2: Is there a significant difference in intercepts? ---------
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(5,4,4,2)+0.1)

attach(drinkData)

plot(wt ~ weeks, type="n")
points(wt[treat == "control"] ~ weeks[treat == "control"], pch=1,col="red")
points(wt[treat == "thyroxine"]~weeks[treat == "thyroxine"], pch=2, col = "blue")
points(wt[treat == "thiouracil"]~weeks[treat == "thiouracil"],pch=3, col = "black")
abline(lm(wt~weeks, data = subset(drinkData, treat == "control")), lty=1,col ="red")
abline(lm(wt~weeks, data = subset(drinkData, treat == "thyroxine")), lty=2, col ="blue")
abline(lm(wt~weeks, data = subset(drinkData, treat == "thiouracil")), lty=3,col ="black")
legend(0,180,c("control","thyroxine","thiouracil"),lty=1:3, pch=1:3,col=c("red","blue","black"))

detach(drinkData)


## Answer(?): No there seems to be no significant difference in fixed effects
# since intercepts among treatments are similar so fixed effects line has no
# significant difference in intercepts. 
## NOTE: this is the difference in intercepts of models: 
# Control: (B0) + B_slope_C * weeks
# Thyroxine: (B0 + B_X) + B_slope_X*weeks
# Thiouracil: (B0 + B_A) + B_slope_A*weeks

summary(drink.lme)
# the coefs of treatThiouracil and treatThyroxine are not significant, proving the claim.
setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/ASSIGNMENTS/A3/plotTools.R")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#library(nlme)
#library(lme4)
#detach(package:lme4)
library(lattice)
library(geepack) # for geeglm()
library(ggcorrplot) # for ggcorrplot
library(forecast) # for ggAcf
library(reshape2)

options(show.signif.stars = FALSE)




# Data: Cognitive Behavior Therapy to Beating the Blues
# Treatment = patients were randomly allocated to either BtheB (beat the blues) program
# or to Treatment as Usual (TAU)
# Drug = Yes/No, whether patient was taking antidepressant during study
# Time = 2 month space
# Response = Y = survey score result. Ranges from 0 to 63 (severe depression)
# These are the cols labelled bdi.pre, bdi.2m, ..bdi.8m (taken at the monthly time intervals)


bluesData.wide <- read.table("data/cbtb.txt", header=TRUE)
head(bluesData.wide)



# part (a) --------------------------------------------------------

# Correlation matrix for the repeated measures bdi.pre ... bdi.8m
# What correlation structure for a GEE is suggested?

# TODO: question teacher: 
corMat <- cor(bluesData.wide[-c(1,2,3)]) 
corMat
diff(corMat[1,][-1])
diff(corMat[2,][-2])
diff(corMat[3,][-3])
diff(corMat[4,][-4])
diff(corMat[5,][-5])
# 
ggcorrplot(corMat)
# Correlation seems to increase with increase in time lag (lag = 2 months) since
# the squares get darker red for increasing months. 


# COR STRUCTURE: Autoregressive since number of patients in BLOCK (i)at time (s) depends
# on those measured at time (s-1), and also less strongly at time (s - 2) and other
# times (t) in the past. 


# part (b) --------------------------------------------------------------------------------

# Reshape data to long format: 

blueColNames = names(bluesData.wide)
bluesData.wide$Subject <- 1:nrow(bluesData.wide)
bluesData <- reshape(bluesData.wide, 
                     varying=list(blueColNames[4:8]), 
                     timevar="Months", 
                     times=c(0, 2, 4, 6, 8), 
                     v.names = "Score", 
                     idvar="Subject", 
                     direction="long")

# Ordering the data by time
bluesData <- with(bluesData, bluesData[order(Subject, Months), ])
bluesData

# Making the Subject name a factor
N <- nrow(bluesData)
bluesData$Subject <- factor(paste(rep('S', N), bluesData$Subject, sep=''))



# part c) ------------------------------------------------------------------------------

# Replications = 5 (5 monthly measures on each Subject)
# N = 31 subjects
length(levels(bluesData$Subject))
# Y = score of the patient
# Treatment = treat (levels: TAU or Beatheblues)
# Grouping factor or Unit: subject
# Time: Months (no need to centre since there is a meaningful 0 value)

# Exploratory plots



# PLOT 1: =============================================================================
# Usable for identifying (1) between-group variation, and (2) within-group variation

# formula = response ~ primaryCovariate | groupingFactor or ~1 |groupingFactor when 
# no other suitable candidate
bluesData.grouped <- groupedData(Score ~ Months | Subject, outer= ~treatment, 
                                 data=bluesData)
plot(bluesData.grouped, outer=~treatment+drug, aspect=0.4)

# Or by ggplot: 

numSubjects <- length(levels(bluesData$Subject))



# INTERPRET:  to see structure of ssytematic and random effects, we are plotting profiles 
# for each experimental unit (subject) across time (months). 

# (i) linearity in response:VERY nonlinear! Lines are concave up in all groups mostly except
# for the Tau/No group, where lines are mostly concave down. 

######## (1) Addressing: VARIABILITY AMONGST GROUPS: (fixed effect interaction)

# TREAT * TIME (fixed effect)
ggplot(data=bluesData, aes(x=Months, y=Score, color=Subject)) + geom_line() + geom_point() +
      facet_wrap(~ treatment, ncol=2) + 
      scale_colour_manual(values=sample(ggplotColors(numSubjects)))
### INTERCEPTS among treatments over time:
# ---> Similar intercepts among the BtheB group and TAU group.
### SLOPES among treatments over time:
# ---> growth rate (slope) is negative for BtheB and curved downward, but for TAU
# group the slope is generally horizontal as time goes on. BtheB group thus shows definite
# decrease in depression as time increases. 
# ====> SUGGESTS strong TREAT*TIME interaction. 

# DRUG * TIME (fixed effect)
ggplot(data=bluesData, aes(x=Months, y=Score, color=Subject)) + geom_line() + geom_point() +
      facet_wrap(~ drug, ncol=2) + 
      scale_colour_manual(values=sample(ggplotColors(numSubjects)))
### INTERCEPTS among drugs over time: 
# ---> similar intercepts
### SLOPES among drugs over time: 
# ---> growth rate is generally average for Drug=No but generally is sharply decreasing
# for Drug=Yes group as months increase.
# =====> SUGGESTS: DRUG * TIME interaction

# TREAT * DRUG * TIME: (fixed effect)
ggplot(data=bluesData, aes(x=Months, y=Score, color=Subject)) + geom_line() + geom_point() +
      facet_wrap(~ treatment + drug, ncol=2) + 
      scale_colour_manual(values=sample(ggplotColors(numSubjects)))
### INTERCEPTS among (Treat=Blues) : (Drug = Yes / No)
# ---> intercepts are higher for Blues group taking Drugs so the antidepressants group has 
# higher depression?  ===> SUGGESTS: intercepts are different. 
### INTERCEPTS among (Treat=TAU) : (Drug = Yes/No) :  ---> intercepts similar
### SLOPES among (Treat=Blues) : (Drug = Yes/No) : ---> similar steepness and concavity
### SLOPES among (Treat=TAU) : (Drug = Yes/No) : ---> concave down for Tau/No and concave up for
# Tau/Yes, so the control group with No drugs has decrease in depression ====> SUGGESTS: there is
# some TREAT*DRUG*TIME interaction



######## (2) Addressing: 
#   VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)
ggplot(data=bluesData, aes(x=Months, y=Score, color=Subject)) + geom_line() + geom_point() +
      facet_wrap(~ treatment + drug, ncol=2) + 
      scale_colour_manual(values=sample(ggplotColors(numSubjects)))

### RANDOM INTERCEPTS: (Space between lines within treat*drug): 
# Within each treatment*drug group there is variability among subjects since the 
# lines are spaced out. Generally more spacing in the TAU than Blues group. 
# =====> suggests some need for random intercepts model: ~ 1|Subject

### RANDOM SLOPES: (Slopes within treat*drug): the rate of increase in depression over time
# (slope) is similar for all subjects in Blues/No,Yes and Tau/Yes but differs for Tau/No
# For Tau/No the liens are concave down while lines are concave up for the rest. 
# ====> suggests random slopes: ~Months | Subject




# PLOT 2: =====================================================================================
# Usable for identifying (1) between-subject variation, and (2) within-subject variation

### INTERCEPTS AND SLOPES 95% CI: 

#### Can visualize the mean response as a weighted average of the individual Subject profiles
# provided that the between-rats variance is included in the averaging. 
# Finding linear response for each Subject using lmList()
# Can help explain random effects when the model is fitted. 
blues.lmlist <- lmList(Score ~ Months | Subject, data=bluesData)
blues.lmlist

# note: for this to work, do NOT load library(lme4) and nlme at the same time
blues.ci <- intervals(blues.lmlist) 
#plot(blues.ci)

# Plotting the intercept and slope Confidence intervals (95%)
intDf <- data.frame(blues.ci[,,1]) # drink.ci[,,"(Intercept)"])
slopeDf <- data.frame(blues.ci[,,2]) # drink.ci[,,"weeks"])
subjectNames <- rownames(intDf) # same for int and slope dfs

ptreat = bluesData$treatment[match(levels(bluesData$Subject), bluesData$Subject)]

ciDf <- data.frame(Estimate=c(intDf$est., slopeDf$est.), 
                   Lower = c(intDf$lower, slopeDf$lower),
                   Upper = c(intDf$upper, slopeDf$upper),
                   Type=c(rep("intercept", nrow(intDf)), rep("slope", nrow(slopeDf))),
                   Subject= subjectNames, 
                   treatment = ptreat)

# ggplot(ciDf, aes(x=Estimate, y=Subject, color=treatment)) + 
#       geom_errorbarh(aes(xmin=Lower, xmax=Upper)) + 
#       geom_line() + geom_point() + facet_wrap(~Type, ncol=2) 
ggplot(ciDf[ciDf$Type == "intercept",], aes(x=Estimate, y=Subject, color=treatment)) + 
       geom_errorbarh(aes(xmin=Lower, xmax=Upper)) + 
       geom_line() + geom_point() 
ggplot(ciDf[ciDf$Type == "slope",], aes(x=Estimate, y=Subject, color=treatment)) + 
       geom_errorbarh(aes(xmin=Lower, xmax=Upper)) + 
       geom_line() + geom_point() 
#INTERPRET: 
# Intercepts: the intercepts vary BETWEEN subjects so suggests random intercepts model
# Slopes = lots of slope variation BETWEEN subjects so suggests random slopes model
# ===> should fit: randintercepts/random slopes model


# ANother way to draw the boxplots all-in-one approach 
# (averaging over the subjects)
slopeByTreatDf <- melt(split(slopeDf$est., ptreat))
colnames(slopeByTreatDf) <- c("slope", "treat")
ggplot(slopeByTreatDf, aes(x=treat, y=slope, color=treat)) + geom_boxplot(size=1)

# (1) VARIABILITY AMONGST GROUPS (fixed effect interaction)

### --- Slopes among treatments:
# For lines plot, the  depression rate (slope) seems steeper in some cases for the TAU
# group compared to Blues group. On the boxplot, can this this as TAU boxplot reaching
# higher in max value than the Blues box. 
# But since boxplots overlap, there is no significant difference in slopes between treatments

# ===> suggests no need for fixed effect interaction term. 
# (can do same thing for the Drug groups (have Drug on x-axis))


# TODO: just keep this part for assignment: +++++++++++++++++++++++++++++++++++++++++++++++++

# (2) VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# ----- Random slopes: (Slopes within treat): the rate of increase in depression over time
# (slope) differs for subjects within a treatment (can see this more clearly than on lines plot)
# For Blues, there is less variation in slopes among subjects than for TAU since boxplot
# for Blues is narrower than for TAU. 

# ====> suggests need for random slopes: ~ Months | Subject
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

intByTreatDf <- melt(split(intDf$est., ptreat))
colnames(intByTreatDf) <- c("intercept", "treat")
ggplot(intByTreatDf, aes(x=treat, y=intercept, color=treat)) + geom_boxplot(size=1)

# (1) VARIABILITY AMONGST GROUPS (fixed effect interaction)
### --- Intercepts among treatments:
# Blues boxplot is lower than TAU boxplot, so can see that intercepts for Blues group
# are generally lower. Not significant since boxplots overlap. 


# (2) VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# ----- Random Intercepts: (Space between lines within treat): 
# Within each treatment group there is variability among subjects since the 
# lines are spaced out. 
# Variation in intercepts among subjects is similar for Blues and TAU group since each of
# their boxplots have similar width. 

# =====> suggests no (?) need for random intercepts model: ~ 1|subject




## PLOT 3: ====================================================================================

# usable for identifying (1) between-group(treat) variation (so just fixed effect interaction)

# Interaction plot of week and treatment: 
interactionPlot(data=bluesData, x.factor="Months", trace.factor="treatment", response="Score")

# INTERPRET: suggests interaction between treat and Time (months) starting from month 2 onwards.
# Difference in slope between Blues and TAU groups seems significant for all months. 
# For Months 0 -2: TAU drops but Blues drops sharply
# For Months 2 - 4: similar slope
# For Months 4 - 6: TAU drops bue Blues goes up. 
# For Months 6 - 8: TAU drops but Blues steadies. 
## Intercepts are similar for TAU and Blues, but vertical distance between the lines over all
# months seems wide. 

# ===> suggests fixed effect interaction: Time * Treat


# Interaction plot of week and Drug: 
interactionPlot(data=bluesData, x.factor="Months", trace.factor="drug", response="Score")
# INTERPRET: different slopes for months 0 - 2 but then similar slopes. 
# Different intercepts since the distance between lines changes. 

# ===>  suggestion for fixed effect interaction: Time * Drug

# Interaction of treat *Drug
interactionPlot(bluesData, x.factor="drug", trace.factor = "treatment", response="Score")
# Strong interaction since slopes are opposite. For TAU from No to Yes there is downward slope
# and upward slope for Blues. 





# part (d) -----------------------------------------------------------------------------------

# TODO: GEE


# part (e) -----------------------------------------------------------------------------------

# (ii) Find appropriate mixed model using lme
# NOTE: must fit using method="ML" to compare models with different fixed effects
# but same random effects.
# Results for final model should be found using REML. 
# ERROR: TODO
blues.lme <- lme(Score ~ drug*treatment*Months, random= ~Months|Subject, data=bluesData, 
                 method="REML")

anova(blues.lme)
# three-way interaction not significant, so discard

# Step 2: fitting without 3-way interaction
blues.alltwoway.lme <- 


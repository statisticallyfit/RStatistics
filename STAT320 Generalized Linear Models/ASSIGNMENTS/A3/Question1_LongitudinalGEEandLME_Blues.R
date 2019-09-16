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
ggplot(data=bluesData, aes(x=Months, y=Score, color=Subject)) + geom_line() + geom_point() +
      facet_wrap(~ treatment + drug, ncol=2) + 
      scale_colour_manual(values=sample(ggplotColors(numSubjects)))


# INTERPRET:  to see structure of ssytematic and random effects, we are plotting profiles 
# for each experimental unit (subject) across time (months). 

# (i) linearity in response:VERY nonlinear! Lines are concave up in all groups mostly except
# for the Tau/No group, where lines are mostly concave down. 

######## (1) Addressing: VARIABILITY AMONGST GROUPS: (fixed effect interaction)

### --- Intercepts among drug/treatments:
# ---> Blues: Intercepts are higher for Drug=Yes sujects than for Drug = No subjects, so 
# depression was higher for anti-depressants group. 
# =====> suggests intercepts are different for drug / treatment lines
# ---> TAU: Intercepts generally similar for Drug = Yes or No groups. 


### --- Slopes among drug/treatments:
# (III) growth rate (slope) is negative for BtheB and curved downward, but for TAU
# group the slope is generally horizontal as time goes on. BtheB group thus shows definite
# decreas in depression, not so starkly evident for TAU. 


######## (2) Addressing: 
#   VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)


# (IV) Random Intercepts: (Space between lines within treat): 
# Within each treatment group there is variability among subjects since the 
# lines are spaced out. 
# =====> suggests some need for random intercepts model: ~|Subject

# (V) Random slopes: (Slopes within treat): the rate of increase in depression over time
# (slope) is similar for all subjects within Blues group but differs widely for TAU, since
# some lines dive downa nd some shoot up and some are average. 
# ====> suggests random slopes: ~Months | Subject


# (vi) Fanning out: differences amongst Subjects at the beginning are preserved throughout
# the trial  for Blues group (since the lines are spaced similarly over time).
# But not true for TAU group. 

## (vii) RESIDUAL Variation: ??? 
# This is the overall within-group (within-subject) variation
# Within each treatment group there is variability among subjects since the 
# lines are spaced out. 
# Boxplot reflects this as wider boxplots for all treatment groups. 







# PLOT 2: ==============================================================================
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


# (2) VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# ----- Random slopes: (Slopes within treat): the rate of increase in depression over time
# (slope) differs for subjects within a treatment (can see this more clearly than on lines plot)
# For Blues, there is less variation in slopes among subjects than for TAU since boxplot
# for Blues is narrower than for TAU. 

# ====> suggests need for random slopes: ~ Months | Subject


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




setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/ASSIGNMENTS/A3/plotTools.R")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
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
# Making the Subject name a factor
N <- nrow(bluesData)
bluesData$Subject <- factor(paste(rep('S', N), bluesData$Subject, sep=''))
bluesData

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#bwide = bluesData.wide
#gs = colnames(bwide)
#gs[4] = "bdi.0m"
#colnames(bwide) = gs

#bd = reshape(bwide, varying=list(gs[4:8]), timevar="Months", times=c(0, 2, 4, 6, 8), 
#             v.names = "Score", idvar="Subject", direction="long")
#bd = with(bd, bd[order(Subject, Months), ])
#N <- nrow(bd)
#bd$Subject <- factor(paste(rep('S', N), bd$Subject, sep=''))
#bd


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


# TODO: just keep this part (all the (2)'s) for assignment: +++++++++++++++++++++++++++++++++++++++++++++++++

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

# =====> cannot say if the width is wide enough to justify need for random intercepts model: ~ 1|subject




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

# COR STRUCTURE: Autoregressive since number of subjects in BLOCK (i)at time (s) depends
# on those measured at time (s-1), and also less strongly at time (s - 2) and other
# times (t) in the past. 
# GROUPING: given by the "id" option. The block referring to (i). The field for birds. 
# Specifies which Subject observations form a block of data. 

# Fitting the GEE model using normal error distribution (so linear model)
blues.gee <- geeglm(Score ~ drug*treatment*Months, id=Subject, corstr="ar1", 
                    family=gaussian, data=bluesData)

summary(blues.gee)
# INTERPRET: the two-way and three-way terms are not
# significant given the other terms are fitted so just remove these terms

# FItting the next GEE model without nonsignificant terms: 
blues.nointeract.gee <- geeglm(Score ~ drug + treatment + Months, id=Subject, corstr="ar1",
                          family=gaussian,data=bluesData)
summary(blues.nointeract.gee)
# INTERPRET: now drug is no  longer significant, so remove it


# Fitting the treat + Months GEE model
blues.final.gee <- geeglm(Score ~ treatment + Months, id=Subject, corstr="ar1", 
                          family=gaussian, data=bluesData)
summary(blues.final.gee)
# INTERPRET: 
# --> Correlation between two sequential (lag=1) observations in the same BLOCK:
# alpha_st = 0.676
# --> treatmentTAU coeff: significant and positive means that TAU group scores higher
# score than BeatTheBlues group, meaning TAU group has significantly higher depression. 
# ---> Months: as months increase, depression decreases since coeff is negative. 



# part (e) -----------------------------------------------------------------------------------


# (i) plot of groupedData() --- please see part (c) for exploratory plots


# (ii) Find appropriate mixed model using lme

### MODEL: 
# Fixed part: the part of the model where Score is predicted by subject's drug, treatment, 
# and Months variable. 
# Random part: variation other than predictors that affect a subject's Score .
# These may cause Score to be higher or lower (random intercepts) or may cause
# the Score to grow at a faster or slower rate (random slopes). 
# This variation can be modeled with a random intercept and slope, respectively, for
# each subject. 
# Also can expect some month-to-month variation WITHIN each subject (residual variance)
# Assumption: this error is homogeneous and uncorrelated. 


# NOTE: must fit using method="ML" to compare models with different fixed effects
# but same random effects.
# Results for final model should be found using REML. 


# STEP 1: obtain the random effects part of the model: ========================================
blues.lme <- lme(Score ~ drug*treatment*Months, random= ~Months|Subject, data=bluesData)

VarCorr(blues.lme) # intercept variance is much larger than months variance (slope) so this
# might suggest just a random intercepts model. 
intervals(blues.lme)
# sd.slopes interval is above zero so sd.slopes is significantly different from zero
# But is also below sd.intercept interval so there is significantly higher intercept
# variation compared to slope variation. 

# Using better anova analysis to test if random slopes is needed: 
blues.intercepts.lme <- lme(Score ~ drug*treatment*Months, random = ~1|Subject, data=bluesData)

anova(blues.intercepts.lme, blues.lme) # rand slopes not significant. 
# INTERPRET: 
# Random slopes not significant so the Subjects do not vary much over months. 


#### ANALYZING random effects part of intercepts model: =======================================

VarCorr(blues.intercepts.DrugMonth.lme)
# var.intercept = 45.77
# var.residual = 48.655       (overall within-subject variation)

# INTERPRET: intercept is smaller or similar than var.weeks (slopes) so this is a good indicator
# that random intercepts is needed. 


# Significance of variance components: 
blues.sigma.ci <- intervals(blues.intercepts.DrugMonth.lme)
blues.sigma.ci

# INTERPRET: 
# (1) sigma.intercept and sigma.residual are overlapping so not statistically different
# from each other ==> suggests intercept is needed just as much as residual error. 

# (2) all Ci are above zero so the standard deviation components  are all significantly 
# different from zero. Since sigma.intercept interval above zero, confirms intercepts model
# is needed. 


# STEP 2: obtain the correct fixed effects part of the model

#### Comparing fixed effects for random intercepts model: we must refit the intercepts model
# using ML to be able to compare models with different fixed effects but same random effects.
blues.intercepts.MLlme <- lme(Score ~ drug*treatment*Months, random = ~1|Subject, data=bluesData,
                              method="ML")
anova(blues.intercepts.MLlme) # shows only fixed effects (between group variation)
# INTERPRET: 3-way interaction is not significant so remove it.

#### Removing the 3-way interaction: 
# NOTE: must say that we tried all possible combinations: fitting treat*Months first second
# and last and in all cases it was non-significant, so can fit it last and then remove it. 
blues.intercepts.alltwoway.lme <- lme(Score ~ drug * treatment + drug*Months + 
                                         treatment*Months, random = ~1|Subject, 
                                      data=bluesData, method="ML")
anova(blues.intercepts.alltwoway.lme)
# INTERPRET: treatment*Months interaction erm is not sigifniicant == remove it. 


#### Removing the treatment*Months interaction:
blues.intercepts.noTM.lme <- lme(Score ~ drug*Months + drug*treatment, random = ~1|Subject,
                                 data=bluesData, method="ML")
anova(blues.intercepts.noTM.lme)
# INTERPRET: the drug*treatment term is not significant, so remove it


#### Removing drug*treat: (leaves only drug*Months interaction)
blues.final.lme <- lme(Score ~ treatment + drug*Months, random = ~1|Subject, 
                               data=bluesData, method="ML")
anova(blues.final.lme)
# INTERPRET: all terms are significant so stop here. The drug*Months interaction is significant
# so must leave it in. Even though main-effects drug term is not significant, we must leave it
# in also since the interaction containing drug is significant. 

# Refitting model using method = REML
blues.final.lme <- lme(Score ~ treatment + drug*Months, random = ~1|Subject, 
                                      data=bluesData)
anova(blues.final.lme)


#### Analyzing the fixed-effects terms: =======================================================
summary(blues.final.lme)

# INTERPRET: (interaction drug*Months significant so do NOT interpret drug,month main effects)

### --> drugYes:Months coeff is significant ==> change in Score over time IS significantly
# different for subjects using drugs versus no drugs. 
# Term is negative ==> significantly lower score in depression for people USING  drugs
# than people not using anti-depressant drugs.

### --> treatmentTAU is significant and positive ==> Score is significantly higher for
# people in TAU group than people in BeatTheBlues group, so that means TAU people have
# higher depression score than the BeatTheBlues group. 




# part (iii) Residuals plots ------------------------------------------------------------------


fixedResids = blues.final.lme$residuals[,1]
df <- data.frame(SubjectResids = blues.final.lme$residuals[,2], 
                 SubjectStdResids = resid(blues.final.lme, type="normalized"), 
                 FixedResids = blues.final.lme$residuals[,1],
                 FixedStdResids = (fixedResids - mean(fixedResids))/sd(fixedResids), 
                 SubjectFitted = blues.final.lme$fitted[,2], 
                 FixedFitted = blues.final.lme$fitted[,1],
                 Subject = bluesData$Subject, 
                 treatment = bluesData$treatment, 
                 drug = bluesData$drug)

###### Residuals vs fitted ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# (1) Residuals vs fitted (by Subject (random part))
ggplot(data=df, aes(x=SubjectFitted, y=SubjectStdResids, color=treatment)) + 
   geom_point(size=2) + 
   geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
   geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
   facet_wrap(~treatment+drug)+
   ggtitle("Residuals vs Fitted (Grouping Factor = subject)")


# (2) Residuals vs fitted (by Age (systematic or fixed part))

ggplot(data=df, aes(x=FixedFitted, y=FixedStdResids, color=treatment)) + geom_point(size=2) + 
   geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
   facet_wrap(~treatment+drug) +
   ggtitle("Residuals vs Fitted for Fixed Line")





######  QQ plot of residuals (fitted) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

shapiro.test(df$FixedResids) # for fixed part of the model, marginally non-normal
shapiro.test(df$SubjectResids) # for subject, there is evidence of non-normality

# (1) Standardized residuals by treat (Fixed part): 
qqnorm(blues.final.lme$residuals[,1]) # fixed

ggplot(df, aes(sample = FixedStdResids)) + 
   stat_qq(color="dodgerblue", size=3, alpha=0.5) + 
   stat_qq_line(linetype="dashed", size=1) + 
   ggtitle("QQnorm plot for Fixed Model")

# (2) Standardized residuals by Subject (random part): 
qqnorm(residuals(blues.final.lme, type="normalized"))

ggplot(df, aes(sample = SubjectStdResids)) + 
   stat_qq(color="dodgerblue", size=2) + 
   stat_qq_line(linetype="dashed", size=1) + 
   ggtitle("QQnorm plot for Subject")



### Residuals vs predictors (using boxplots) +++++++++++++++++++++++++++++++++++++++++++++++++++

# (1)  Boxplot of residual vs predictor (by treat (fixed part))
ggplot(df, aes(x=treatment, y=FixedStdResids, colour=drug)) + geom_boxplot(size=1) + 
   geom_hline(yintercept=0, linetype="dashed",color="black",size=1)
# non-homogeneity of variance though all centred around mean = 0
# also some outliers

# (2) Boxplot of residual vs predictor (by subject (random part))

#plot(blues.final.lme, Subject ~ resid(., type="p"),abline=c(0,1))

ggplot(df, aes(x=Subject, y=SubjectStdResids, colour=treatment)) + geom_boxplot(size=1)  +
   geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
      ggtitle("Subject vs Standardized Subject Residuals")
# non-homogeneity of variance, with outliers, and general non-centering around y = 0 line. 



# part (iv, v) --------------------------------------------------------------------------------
# AUTOCORRELATION plot

# ACF for all residuals combined
bluesResids <- blues.final.lme$residuals[,2] # residuals for random part only (Subject)
acf(bluesResids) 
ggAcf(bluesResids)

# INTERPRET: 
# ACF does not appear to have a random distribution 
# Some autocorrelations are greater than 2 stdevs (dotted lines), and sinusoidal pattern.
# ===> suggests a systematic effect may be missing. 



# ACF for residuals by subject
subject.level <- levels(bluesData$Subject)

# Getting default plot values
# par("mfrow", "oma", "mar")
# $mfrow
# [1] 1 1
# $oma
# [1] 0 0 0 0
# $mar
# [1] 5.1 4.1 4.1 2.1
par(mfrow=c(3,3), mar=c(2,4,0,0), oma=c(2,2,4,2))

for(i in seq(along=subject.level)){
   datain <- bluesData$Subject == subject.level[i]
   
   acf(bluesResids[datain], xlab="", ylab="", main=paste("Subject", subject.level[i]))
   #print(ggAcf(drinkResids[datain])) + ggtitle(paste("rat", rat.level[i]))
}

# ACF by subject seem to follow no pattern, and do not have outliers. Harder to discern
# any pattern since there are only 4 observations (excluding lag 0) per Subject. 

par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(5,4,4,2)+0.1)


# part (vi) ----------------------------------------------------------------------------------
summary(blues.final.lme)

# part (f) ----------------------------------------------------------------------------------
 
# Compare GEE and and LME, examining parameter estimates and their standard errors. 

summary(blues.final.gee)
summary(blues.final.lme)

# SIMILAR: both have same conclusions for treatmentTAU and Months predictor coeffs: 
# Months: as time goes on depression falls
# treatmentTAU: depression for TAU group is higher than for BeatTheBlues group. 

# Standard errors: higher in general for the LME model than for GEE model
# Coefs: similar values in predictor coefs
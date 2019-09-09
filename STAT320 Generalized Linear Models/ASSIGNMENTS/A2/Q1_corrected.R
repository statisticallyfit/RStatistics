setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
library(lme4)
library(lattice)

options(show.signif.stars = FALSE)


# part (a) -----------------------------------------------------------------------
# Consructing the data set

Trig <- scan('data/trigl.txt')

Method <- factor(rep(c("M1", "M2"), each=8))
Day <- factor(rep(c("D1", "D2", "D3", "D4"), each=2, times=2))

trigData <- data.frame(Trig=Trig, Method=Method, Day=Day)
trigData

# part (b) -----------------------------------------------------------------------
# Exploratory plot and summraize information

# Day within method variance component ( for a nested design) 

### Variation among Day within Mehod: little variation in D4 for Method 1 and 2
# but there is larger variation for D2,D3 for Method 1 and for D1, D3 in Method 2. 
# Overall, within each method, there is larger variability in Triglyceride level
# for Day1,2,3 and lower variability in triglyceride level for Day 4. 
# bwplot(Trig ~ Day | Method, data=trigData) # Day within method

### CORRECTION: not of interest since Day is a random variable. 
### FOCUS POINT: focus on variabiliy among Day 
### Reference: see worksheet 5 question 1: they write Type | Subject
# to study VARIABILITY AMONG SUBJECTS (where Subject = random effect, Type = fixed effect)

# VARIABILIY AMONG DAYs: (Days = random effect = on title and Method on x-axis)
bwplot(Trig ~ Method | Day, data=trigData)


# DIFFERENCE IN MEAN TRIG LEVEL OVER METHOD: 
# If we averaged across Day within each Method, we  see
# that the average eye intensity differs between Methods 1, 2. In particular, 
# the mean triglyceride level for Method 1 is higher than for Method 2. 
# This is the Method variance component. 
bwplot(Trig ~ Method , data=trigData) 


# INTERPRET: 
### Variation among labs: see that lab 4 is different than other labs.
### Variation among batches WITHIN LABS: see that there is little variation among
# batches within labs since L1, L2, L3 seem to have the same variation per batch
# only L4 and L6 seem to have different variation per batch, especially at batch
# B1, B2 for Lab4. 

# WRONG: must focus on variability among days not variability among methods
#dotplot(Day ~ Trig | Method, data=trigData, pch=c(1,1,2,2,3,3,4,4),
#        strip=FALSE, strip.left=TRUE, layout=c(1,2), cex=1.5, 
#        ylab="day within method", xlab = "triglyceride level", jitter.y = TRUE)

# CORRECT: focus on variability among days: (similar to boxplot of method
# averaged over day)
dotplot(Method ~ Trig | Day, data=trigData, pch=c(1,1,2,2),
        strip=FALSE, strip.left=TRUE, layout=c(1,4), cex=1.5, 
        ylab="method within day", xlab = "triglyceride level", jitter.y = TRUE)

# INTERPRETATION: 
# Dotplot suggests a difference in mean trig levels between methods, with Method 1
# resulting in higher mean Trig level than Method 2

# School way
# dotplot(Day ~ Trig, groups=Method, ylab="Day", cex=2,
#       auto.key=list(x=0.8,y=0.33, cex=1.4, title="Method",cex.title=1.5), data =trigData)



### VARIABILITY AMONG DAYS: 

# Suggests maybe interaction between Day and method since slope from D1 to D2
# for M1 is not the same for M2. However the slopes are similar for the other
# levels for Day. 
# School INTERPRET: similar drop in trig level on Day 2 for both methods
# but when using M2 there is an increase in mean trig levels from Da 3 to 4, 
# while there is a corresponding drop for Method 1. Suggests interaction
# between Day and Method since change in trig levels across days is NOT
# the same for each method. 
with(trigData, interaction.plot(x.factor=Day, trace.factor=Method, response=Trig))


#  part (d) ----------------------------------------------------------------------------

# notation: Worker/Machine means Worker + (Machine-within-worker) : ~1| Worker/Machine
# notation: seed/fertz means seed + (fert-within-seed) : ~1 | seed/fert

# Mixed model with random effect interaction. (interaction is a random effect since
# Day is random )
trig.lme <- lme(Trig ~ Method, random = ~1 | Day/Method, data=trigData)
trig.lmer <- lmer(Trig ~ Method + (1|Day/Method), data=trigData)
summary(trig.lmer)
summary(trig.lme)
VarCorr(trig.lme)


# INTERPRET: 
sigma.methodInDay <- 4.85764
sigma.residual <- 3.797038
sigma.day <- 4.533303


# Significance of variance components: 
intervals(trig.lme)
# INTERPRET: 
# No Ci's are significantly different from each other since they each overlap. 




# Fit the non-interaction model

trig.nointeraction.lme <- lme(Trig ~ Method, random = ~1 | Day, data=trigData)
trig.nointeraction.lmer <- lmer(Trig ~ Method + (1|Day), data=trigData)
summary(trig.nointeraction.lmer)
summary(trig.nointeraction.lme)



# part (e) ------------------------------------------------------------------------------

# Which model is preferred: 
anova(trig.nointeraction.lme, trig.lme)
# Going by p-value: the interaction term is not important
# Going by AIC: the interaction model has lower AIC but the delta is small so choose the
# more parsimoniuous model and p-value is in favor of the simpler model too. 


# part (f) -------------------------------------------------------------------------------

# Write the terms in the final model: 

summary(trig.nointeraction.lme)
random.effects(trig.nointeraction.lme)

# fixed effects line: 147.000 - 0.975 * MethodM2
# random effects line: 1.367301*D1 - 7.120095*D2 + 1.954734*D3 + 3.798059

# part (g) -------------------------------------------------------------------------------

# Get the variance components of the final model : 
VarCorr(trig.nointeraction.lme)


# INTERPRET: 
# sigma.residual <- 5.22383
# sigma.day <- 5.39736


# Significance of variance components: 
intervals(trig.nointeraction.lme)
# INTERPRET: 
# No Ci's are significantly different from each other since they each overlap. 


# part (h) -------------------------------------------------------------------------------

# RESEARCH question: do the two methods differ?

# Using the non-interaction model, looking at fixed effects we see that p-value for
# the MethodM2 coeff is 0.0052 so there is a significant difference between M2 and M1. 
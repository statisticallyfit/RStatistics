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


# LONGITUDINAL / REPEATED MEASURES DESIGN: (same as matched pairs or dependent t-test)

# In repeated measures designs, there are several individuals (or units) and 
# measurements are taken repeatedly on each individual. When these repeated measurements
# are taken over time, it is called a longitudinal study or, in some applications, a panel
# study. Typically various covariates concerning the individual are recorded and the
# interest centers on how the response depends on the covariates over time. Often it is
# reasonable to believe that the response of each individual has several components: a
# fixed effect, which is a function of the covariates; a random effect, which expresses
# the variation between individuals; and an error, which is due to measurement or 
# unrecorded variables.


# SAME IDEA AS IN TOPIC 10 (HIERARCHICAL MODELS): 
# Idea: grouped data. Each  random effect consists of a group of 
# repeated measures. 
# There are two levels of variability:
# (1) measurement error on each individual within a group. 
# (2) variability amongst groups


# Example of longitudinal data: Scenario 2: 
# Five animals and these same five are used at each treatment level. 
# They are given three different diets or have a reasponse measured at 
# three different time periods. Then the scucessive measurements across time
# periods are not independent, necessarily. 
# GOAL: model between animal variation. 
# Longitudinal design allows increased precision in the estimation of differences
# between treatments when the variation between experimental units is large. 

# Data is given in WIDE FORM with repeated measures written across the columns
flagleafData.wide <- read.table("data/flagleaf.txt", header=TRUE)
flagleafData.wide

# Convert to LONG format
colNames <- names(flagleafData.wide)
flagleafData.wide$ID <- 1:dim(flagleafData.wide)[1] # num rows indexing

flagleafData <- reshape(flagleafData.wide,
                        varying = list(colNames[3:15]),
                        timevar="Time",
                        times=c(11:20,22,25,29), 
                        v.names = "count", 
                        idvar="ID", direction="long")
flagleafData




### Example 12.1 --------------------------------------------------------------------

# DATA:measures of weights of 16 rats over time. Eight rats were given control diet, 4 rats
# were given one experimental diet and 4 were given another diet. 

# Unit = rat (grouping factor)
# Time (primary covariate)
# Treatment: Diet
### Random part: Unit x Time = ~Time | Rat part (random effects from how units react over time)
### Fixed part: Treatment x Time = Diet * Time

data("BodyWeight")
head(BodyWeight)

BodyWeight$Rat
BodyWeight$Diet

# Cleaning up data
N <- nrow(BodyWeight)
ratData <- BodyWeight
ratData$Rat <- factor(paste(rep('R', N), BodyWeight$Rat, sep=''))
ratData$Diet <- factor(paste(rep('D', N), BodyWeight$Diet, sep=''))
ratData

### Grouped data is essential in longitudinal data: data from each rat form a group
# so there are 16 groups of data. 
# The "outer" group is the Diets (Treatment). 

# formula = response ~ primaryCovariate | groupingFactor
# Can write number (1) instead of primaryCovariate when no other suitable candidate.

# Unit = rat (grouping factor)
# Time (primary covariate)
# Treatment: Diet
### Random part: Unit x Time = ~Time | Rat part (random effects from how units react over time)
### Fixed part: Treatment x Time = Diet * Time

groupedRat <- groupedData(weight ~ Time | Rat, data=ratData, outer = ~Diet)
#plot(ratData, outer= ~Diet, aspect=3)
plot(groupedRat, outer= ~Diet, aspect=3) # apect = 3 to have 3 cols

# plot(groupedRat, outer= T, aspect=3)



# INTERPRET: to see structure of ssytematic and random effects, we are plotting profiles for
# each experimental unit across time. 

# (i) response is linear over time for all rats
### --- Intercepts:
# (ii) diets 2 and 3 produce heavier rats than diet 1 (intercepts)
### --- Slopes:
# (iii) growth rate (slope) for diet 1 is less than for diets 2 and 3. 
### ---- Within-diet variability: 
# (iv) Variability in diets 2 and 3 is greater than in diet 1 (since lines are further apart
# in diets 2 and 3 than for diet 1)
# (v) within-diets slopes are similar
# (vi) differences amongst rats at the beginning are preserved throughout the trial 
# (since the lines are spaced similarly over time)



### INTERCEPTS ANS SLOPES 95% CI: 

#### Can visualize the mean response as a weighted average of the individual rat profiles
# provided that the between-rats variance is included in the averaging. 
# Finding linear response for each rat using lmList()
# Can help explain random effects when the model is fitted. 
rat.lmlist <- lmList(weight ~ Time | Rat, data=ratData)
rat.lmlist
rat.ci <- intervals(rat.lmlist) # note: for this to work, do NOT load library(lme4) and nlme at the same time
plot(rat.ci)


# Fitting the model: 

# Unit = rat (grouping factor)
# Time (primary covariate)
# Treatment: Diet
### Random part: Unit x Time = ~Time | Rat part (random effects from how units react over time)
### Fixed part: Treatment x Time = Diet * Time
rat.lme <- lme(weight ~ Diet * Time, data=ratData, random = ~Time|Rat)
rat.nodiff.lme <- lme(weight ~ Diet/Time-1,data=ratData, random= ~Time|Rat)

summary(rat.lme)
summary(rat.nodiff.lme)


# INTERPRET: 

# FIXED EFFECTS: 
anova(rat.lme)
# Diet term is significant, so differences amongst diets (between D2, D1 etc) 
# are significant
# Slopes: also interaction of Diet*Time is significant ==> rates of change differ
# significantly amongst Diets. 
# Also:
summary(rat.nodiff.lme)$tTable


# RANDOM EFFECTS:
rat.lme$coefficients$fixed
rat.lme$coefficients$random


# -----------------------------------------------------------------------------------

# Example 12.3: Chick Data

data(ChickWeight)
head(ChickWeight)

# Unit = Chick (grouping factor)
# Time (primary covariate)
# Treatment: Diet
### Random part: Unit x Time = ~Time | Chick part (random effects from how units react over time)
### Fixed part: Treatment x Time = Diet * Time

# Grouped data: response ~ Time | Unit, outer = ~Treatment
chickData <- groupedData(weight ~ Time | Chick, data=ChickWeight, outer = ~Diet)
N <- nrow(ChickWeight)
chickData$Chick <- factor(paste(rep('C', N), chickData$Chick, sep=''))
chickData$Diet <- factor(paste(rep('D', N), chickData$Diet, sep=''))
chickData


# Plotting the grouped data, say outer = ~Diet or outer = TRUE so that the outer = ~Diet
# we assined when creating the grouped data shows up in the tabs. 
plot(chickData, outer=~Diet, aspect=2)

# INTERPRET: 
# slopes and intercepts are similar, though steeper slope for Diet 3
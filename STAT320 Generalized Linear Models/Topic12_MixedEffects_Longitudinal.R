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


# Fitting the model: random slopes and random interceps: random = ~Time|Rat

# Unit = rat (grouping factor)
# Time (primary covariate)
# Treatment: Diet
### Random part: Unit x Time = ~Time | Rat part (random effects from how units react over time)
### Fixed part: Treatment x Time = Diet * Time

## Fitting the fixed-effects interaction model + randomslopes/random intercepts model
rat.lme <- lme(weight ~ Diet * Time, data=ratData, random = ~Time|Rat)
# Fitting no intercepts model for easier interpretation of differences
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



### CHECKING MODEL DIAGNOSTICS: ---------------------------------

### type="pearson" produces standardized Pearson residuals. 

fr = rat.lme$residuals[,1]
df <- data.frame(RandResids=rat.lme$residuals[,2], 
                 RandStdResids=resid(rat.lme, type="normalized"), 
                 FixedResids=rat.lme$residuals[,1],
                 FixedStdResids = (fr - mean(fr))/sd(fr), 
                 RandFits=rat.lme$fitted[,2], 
                 FixedFits = rat.lme$fitted[,1],
                 Rand = ratData$Rat, 
                 Fixed = ratData$Diet)

# Residuals vs fitted -----------------------------------------------------------------

# (1) Residuals vs fitted (by Rat (random part))
ggplot(data=df, aes(x=RandFits, y=RandStdResids, color=Rand)) + geom_point(size=2) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
      geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
      ggtitle("Residuals vs Fitted for Grouping Factor = Rat")

# This is an interaction model so we plot residuals by variable 1 (Gender)
# Residuals vs fitted (by Gender)
plot(rat.lme, resid(., type="p") ~ fitted(.) | Rat, abline=0)


# (2) Residuals vs fitted (by Age (systematic or fixed part))

ggplot(data=df, aes(x=FixedFits, y=FixedStdResids, color=Fixed)) + geom_point(size=2) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="red") + 
      facet_grid(. ~Fixed) + 
      #facet_wrap( ~ageC, ncol=2) + 
      ggtitle("Residuals vs Fitted for Fixed Line")



### NOTE: the lecture notes does the Random Resids (Rat) but colord/partitioned by Diet: 

plot(rat.lme, resid(., type="p") ~ fitted(.) | Diet, abline=0)

ggplot(data=df, aes(x=RandFits, y=RandStdResids, color=Fixed)) + geom_point(size=2) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
      geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
      #facet_grid(. ~ageC) + 
      facet_wrap( ~Fixed, ncol=3) + 
      ggtitle("Residuals vs Fitted for Grouping Factor = Rat")


### QQ plot of residuals (fitted) ----------------------------------------------------

shapiro.test(df$FixedResids) # for Diet
shapiro.test(df$RandResids) # for Rat, evidence to reject normality

# (1) Standardized residuals by Age (Fixed part): 
qqnorm(rat.lme$residuals[,1])

ggplot(df, aes(sample = FixedStdResids)) + 
      stat_qq(color="dodgerblue", size=3, alpha=0.5) + 
      stat_qq_line(linetype="dashed", size=1) + 
      ggtitle("QQnorm plot for Fixed Line Standardized Residuals")

# (2) Standardized residuals by Subject (random part): 
qqnorm(residuals(rat.lme, type="normalized"))

ggplot(df, aes(sample = RandStdResids)) + 
      stat_qq(color="dodgerblue", size=3, alpha=0.5) + 
      stat_qq_line(linetype="dashed", size=1) + 
      ggtitle("QQnorm plot for Random Effect Standardized Residuals")



### Residuals vs predictors (using boxplots) -------------------------------------------

# (1)  Boxplot of residual vs predictor (by Diet (fixed part))
ggplot(df, aes(x=Fixed, y=FixedStdResids, colour=Fixed)) + geom_boxplot(size=1) + 
      geom_hline(yintercept=0, linetype="dashed",color="black",size=1)
# homogeneity of variance and all centred around mean = 0

# (2) Boxplot of residual vs predictor (by Rat (random part))

plot(rat.lme, Rat ~ resid(., type="p"),abline=c(0,1))

## CORRECTED: color by Fixed var (Diet) not by Rand (Rat), which is already on x-axis!
ggplot(df, aes(x=Rand, y=RandStdResids, colour=Fixed)) + geom_boxplot(size=1) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black")

#ggplot(df, aes(x=Rand, y=RandStdResids, colour=Rand)) + geom_boxplot(size=1) + 
#      geom_hline(yintercept=0, linetype="dashed", size=1,color="black")
# several outliers and no homogeneity of variance




### AUTOCORELATION ------------------------------------------------------------------

# Checking autocorrelation of the Random part only (using rat data, so random = Rat)
theRatResids = df$RandResids
rat.lme$residuals[,2]
# the same

rat.level <- levels(ratData$Rat)
rat.level

# Default value sof par
par("mar", "oma", "mfrow", "las")

# changing them: 
par(mfrow=c(4,4), mar=c(2,4,0,0), oma=c(2,2,4,2))

# Plotting autocorrelations for the current rat level. 
for(i in seq(along=rat.level)){
   dataIn <- ratData$Rat == rat.level[i]
   
   acf(theRatResids[dataIn], xlab="", ylab="", 
       main=paste("rat", rat.level[i], las=1))
}

# Default values of par
# par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1), oma=(0,0,0,0), mgp=c(3, 1, 0), las=0)


## INTERPRET: Autocorrs for rat: 
# The plot of autocorrelation (p_k vs lag k) for each ra indicate there is no need to 
# fit extra components in the error model to account for autocorrelation. 
# Reason: the dotted lines signify 2 standard errors and none of the autocorrs for
# lag > 0 exceed these bounds ==> no remaining info in residuals. 



# -----------------------------------------------------------------------------------

# Example 12.3: Chick Data
par(mar=c(5.1, 4.1, 4.1, 2.1), mfrow=c(1,1), oma=c(0,0,0,0), mgp=c(3, 1, 0), las=0)

data(ChickWeight)
head(ChickWeight)

# Unit = Chick (grouping factor)
# Time (primary covariate)
# Treatment: Diet
### Random effect: Unit x Time = ~Time | Chick part (random effects from how units react over time)
### Systematic effect: Treatment x Time = Diet * Time

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
# There is Chick x Time interaction since the lines for chicks fan out (separate)
# as time increases. Means distance between chick weights for different chicks
# is not the same over time. 

# Observe: in Diets 1 and 4 the lighted chick drops out before the trial is compelte; we
# can tell because the lowest line (lightenst chick) does not complete its line. 
# This can affect the mean profile because the upward trend may be due more to 
# the remaining chicks than due to the effect of Diet over Time. 
# Shortly, there might be bias due to dropouts. 


# Xyplot: gives clearer view of the systematic effect
xyplot(weight ~ Time | Diet, data=chickData, 
       panel=function(x, y){panel.xyplot(x,y);panel.loess(x,y)})

# INTERPRET: systematic effect of time within Diet is curved or piecewise linear
# with change point at about 12 days. 



# Fit random intercepts/slopes model: Time | Chick + fixed interaction model of Diet*Time
chick.lme <- lme(weight ~ Diet*Time, data=chickData, random=~Time|Chick)



# AUTOCORRELATION for chick data: (using the random part Residuals)
chick.acf <- acf(chick.lme$residuals[,2])
plot(chick.acf)

# INTERPRET: Problem!
# (1) the serial correlations are outside the (-2,2) std errors bounds, so outside the
# assumption of independence
# (2) pattern: there is sinusoidal pattern
# ----> so there is remaining systematic effect in residuals that the model hasn't captured. . 


### Example 12.4--- Deer Energy ----------------------------------------------------

# Data: repeated measures of metabolizable energy of 2 strains of deer x male,female
# where the objective was to use the profile to find weeks where body temperature
# was maximum. 

deerData <- read.table("data/ME.txt", header=TRUE)
colnames(deerData) <- c("Tag", "Strain", "Gender", "Age", "AgeInWeeks", "MetabEnergy")
deerData$Gender <- factor(deerData$Gender, labels=c("Male", "Female"))
deerData$Strain <- factor(deerData$Strain, labels=c("Red", "PD"))
deerData$Tag <- factor(deerData$Tag)
head(deerData)

# Y = metabolizable energy (ME)
# 
groupedDeer <- groupedData(MetabEnergy ~ AgeInWeeks | Tag, data=deerData, 
                           outer = ~Gender * Strain)
#trellis.device(color=F, height=6, width=7)
plot(groupedDeer, outer=TRUE)

# INTERPRET: 
# Systematic part: can be represented by a spline curve. 
# Variability amongst animals (Tag) can be explained by a random intercept. 


# Fitted profiles for this model: 
library(splines) # for the bs() function

# Fitting the fixed-effects interaction model with random intercept: ~1|Tag
deer.lme <- lme(MetabEnergy ~ Strain*Gender * bs(AgeInWeeks, df=5), 
                random= ~1 | Tag, data=deerData)
deer.lme







##### AUTOCORRELATION for chick data: 

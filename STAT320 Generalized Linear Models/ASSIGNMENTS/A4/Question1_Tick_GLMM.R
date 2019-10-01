setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/ASSIGNMENTS/A4/plotTools.R')


library(ggfortify)
library(lme4) # for glmer() for fitting GLMM model
library(nlme)
#detach(package:nlme)
library(lattice)
library(car) # for Anova() for testing glmer

options(show.signif.stars = FALSE)


# Data: 
# FIXED effects: Year, Alt
# RANDOM effects: Location, Brood, ID (subject)

tickData <- read.table("data/ticka.txt", header=TRUE)
head(tickData)

# Make ID a factor: 
N <- nrow(tickData)
tickData$id <- factor(paste0(rep('S', N), tickData$ID))
tickData$brood <- factor(tickData$BROOD)
tickData$location <- factor(tickData$LOCATION)
head(tickData)
#tickData$logNumTicks <- log(tickData$TICKS)

# part a) ------------------------------------------------------------------------------------

# Exploratory plot of natural log of number of ticks against altitude, grouped by year
#numSubjects <- length(levels(tickData$ID))

ggplot(tickData, aes(x=ALT,y=log(TICKS), colour=YEAR)) +
      stat_sum(aes(size=..n..),alpha=0.8)+
      #scale_y_log10()+
      scale_size_continuous(breaks=c(2,6,10),range=c(2,7))+
      geom_smooth(method="glm",method.args=list(family=quasipoisson)) + facet_wrap(~YEAR)

# (1) Data is more linear when plotted on log scale: suggests ticks grow exponentially
# with altitude. 
# (2) seemst to be a fixed effect interaction between years since the average slopes are
# different across years. 
# (3) more variation in number of ticks for year 2 and least for year 3, so there is high
# within-subject variation for group Year 2 ===> suggests random intercepts. 


# PLOT 2: =====================================================================================

# Nested design plots: 

### DIFFERENCE BETWEEN NESTED vs. CROSSED DESIGN: 
# Nested design: DIFFERING ID's are within Brood , differing BRood within Year
# Crossed design: the SAME ID's are within Brood, the same Brood are within Year 


# PLOT 3: =====================================================================================
# Fixed effects Interaction

tickData$logTicks <- log(tickData$TICKS)
interactionPlot(x.factor="ALT", trace.factor = "YEAR", response="logTicks", data=tickData)
# clear slope interaction and intercepts are different. 

# part b) -----------------------------------------------------------------------------------

# Centering ALTITUDE: 
tickData$ALT.centred <- tickData$ALT - mean(tickData$ALT)

tickData$ALT.scaled <- scale(tickData$ALT)

# part c) -----------------------------------------------------------------------------------

# (i) testing for significance of random effects

# NOTE: got convergence error when fitting location and id as numerical variables.
tick.randSaturated.glmer <- glmer(TICKS ~ YEAR * ALT.scaled + (1|LOCATION /BROOD /ID), 
                              family=poisson, data=tickData, 
                    control=glmerControl(optimizer="bobyqa",check.conv.grad=.makeCC("warning",1e-3) ) )
#tick.f <- glmer(TICKS ~ YEAR * ALT.centred + (1|location/brood/id), 
#                              family=poisson, data=tickData, 
#                              control=glmerControl(optimizer="bobyqa",check.conv.grad=.makeCC("warning",1e-3) ) )

# Produce table of saturated summary model to show variance components equal division
summary(tick.randSaturated.glmer)



# Random interaction: Location * Brood
tick.randLocBrood.glmer <- glmer(TICKS ~ YEAR * ALT.scaled + (1|LOCATION /BROOD), 
                                 family=poisson, data=tickData, 
                              control=glmerControl(optimizer="bobyqa",
                                                   check.conv.grad=.makeCC("warning",1e-3) ) )

# Random interaction: Location * ID
# NOTE: got convergence error when fitting location and id as numerical variables. 
tick.randLocID.glmer <- glmer(TICKS ~ YEAR * ALT.scaled + (1|LOCATION/ID), 
                              family=poisson, data=tickData, 
                              control=glmerControl(optimizer="bobyqa",
                                                   check.conv.grad=.makeCC("warning",1e-3) ) )

# Random interaction: Brood * ID
tick.randBroodID.glmer <- glmer(TICKS ~ YEAR * ALT.scaled + (1|BROOD/ID), 
                              family=poisson, data=tickData, 
                              control=glmerControl(optimizer="bobyqa",
                                                   check.conv.grad=.makeCC("warning",1e-3) ) )

anova(tick.randLocBrood.glmer, tick.randSaturated.glmer)
anova(tick.randLocID.glmer, tick.randSaturated.glmer)
anova(tick.randBroodID.glmer, tick.randSaturated.glmer) # seem most equivalent

#anova(tick.randLocBrood.glmer, tick.randLocID.glmer, tick.randBroodID.glmer, tick.randSaturated.glmer)

# Next step: reducing the brood-id random effects model further
tick.randBrood.glmer <- glmer(TICKS ~ YEAR * ALT.scaled + (1|BROOD), family=poisson, data=tickData,
                              control=glmerControl(optimizer="bobyqa",
                                                   check.conv.grad=.makeCC("warning",1e-3) ))

tick.randID.glmer <- glmer(TICKS ~ YEAR * ALT.scaled + (1|ID), family=poisson, data=tickData,
                              control=glmerControl(optimizer="bobyqa",
                                                   check.conv.grad=.makeCC("warning",1e-3) ))

anova(tick.randBrood.glmer, tick.randBroodID.glmer)
anova(tick.randID.glmer, tick.randBroodID.glmer)

# INTERPRET: significant nested effect in randbroodid model, so keep that


# (ii) testing for interaction between fixed effects (refining the brood-id model)

Anova(tick.randBroodID.glmer)
# INTERPRET: all fixed effects significant, including the interaction term, so just keep this model.

# NOTE: based on forum advice from teacher do not use anova to compare fixed effects parts
# since we cannot fit using method = "ML" in glmer. 

# part d) -------------------------------------------------------------------------------------------

# Summarize preferred model: 
summary(tick.randBroodID.glmer)

# (i) interpret the estimates of fixed effects

# ---> Intercept: for mean altitude value, for year 1, the value of log(numTicks) is 
# 0.406 (or the number of ticks is exp(0.406) = 1.500803)
# ---> Year * ALT.centred: 
### ---> Year2:Alt.centred: (coef = 0.018, p-value = 0.009) so as altitude increases,
# tehre is statistically fisngificant higher slope for year2 than year1. Means: 
# (1) for a unit increase in altitude, the log(numticks) increases by 0.018 more ticks for year 2 than for year 1. 
# (2) for a unit increase in altitude, the numTicks increases by a factor of exp(0.018) = 1.018 for year2 than for year1
# Year 2 line vs Year 1 line

### ---> Year 3: Alt.centred: (coef = 0.0139, p-value = 0.07) so as altitude increases, there is
# not a statistically significant higher slope for year 3 than year 1. 

# Now cannot interpret main effects if interactions are significant. 


# (ii) if ALT was not centered, then it would mean value of log(numTicks) at altitude 0
# or sea level, for Year 1




# (iii) Interpret relative importance of variance components. 
summary(tick.randBroodID.glmer)$varcor # use summary table only copy varcor part

# INTERPRET: higher brood variance component than id-within-brood variance. 
# So greater source of variability from brood than from individual chicks. 



# part e)---------------------------------------------------------------------------------------------------


# Identify observation with largest residual in absolute value. 

# NOTE: deviance residuals are normally distributed. 
df <- data.frame(Residuals = residuals(tick.randBroodID.glmer, type="deviance"), 
                 Fitted = fitted(tick.randBroodID.glmer), Location = tickData$location, 
                 Brood = tickData$brood)

# (1) Residuals vs fitted --------------------------------------------------------------------

ggplot(df, aes(x=Fitted, y=Residuals)) + geom_point(size=2) +
   geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
   geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") 

# (2) QQPLOT ---------------------------------------------------------------------------------

# Very nonnormal but at least no outliers
ggplot(df, aes(sample = Residuals)) + 
   stat_qq(color="dodgerblue", size=2) + 
   geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
   stat_qq_line(linetype="dashed", size=1)


# Shapiro Test
shapiro.test(df$Residuals) # very non-normal


# Outlier with max residual term: 
sort(abs(df$Residuals))

which.max(abs(df$Residuals))
# observation 304 has the maximum residual of 2.287, which is out of normal bounds

# Which characteritsic of the chick: 
tickData[300:310, ]
# (1) high altitue = 70.759 centred
# (2) ticks num = 2 (among histest, with max = 3)

ggplot(tickData, aes(x=ALT,y=1+TICKS, colour=YEAR)) +
   stat_sum(aes(size=..n..),alpha=0.7)+
   scale_y_log10()+
   scale_size_continuous(breaks=c(2,6,10),range=c(2,7))+
   geom_smooth(method="glm",method.args=list(family=quasipoisson)) + 
   geom_point(data=tickData[304, ], aes(x=ALT, y=1+TICKS, colour="red", size=4)) + 
   facet_wrap(~YEAR)
# From plot, the 304 observation has particularly high number of ticks for its altitude
# in the year3 group. All other observations from year 3 at its altitude have
# fewer number of ticks

# Checking numerically: 
sub = subset(tickData, ALT >= 500 & YEAR == "Y3")
sub[order(sub$TICKS),]  #see, observation 304 is last, with maximum number of tick s= 2
# while others for year 3 have tick = 0 or 1. 


# NOTE: offset is only needed when number of response counts is not the same per
# time or area. Here there are equal numbers of TICKS per each year category. (not true)
y1 = subset(tickData, YEAR == "Y1")
y2 = subset(tickData, YEAR == "Y2")
y3 = subset(tickData, YEAR == "Y3")
nrow(y1)
nrow(y2)
nrow(y3)

setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
#detach(package:lme4)
#detach(package:nlme)
library(nlme)
library(lme4)
library(lattice)

options(digits=8)
options(show.signif.stars = FALSE)

# Data: measures of effort required by 9 subjects to arise from each of 
# the 4 stool types
# Design: randomized block with each subject being a block. 

# RANDOM EFFECT: subject (since it is a random sample for a population)
# FIXED EFFECT: Type of stool (since we investigate it at fixed levels, not random sampled)

# **** NOTE ***: there must be replicate (repeat) measurements from subjects
# in order to estimate random subject effects (coefficients). 
data("ergoStool")

head(ergoStool)

N <- nrow(ergoStool)
ergoStool$Subject <- factor(paste0(rep('S', N), ergoStool$Subject)) 
ergoStool$Type


# Exploratory plots


# Interaction between Type fixed and SUbject random? 
interactionPlot(data=ergoStool, x.factor="Type", trace.factor="Subject", response="effort")

# VARIABILITY AMONG SUBJECTS: 
# Subject 7 has lowest effort for type 1 stool, similar effort for T2,T3 and
# a bit lower effort for T4 stool. Subject 2 has higher effort than all the other
# subjects, for all stool types. 
bwplot(effort ~ Type |Subject, data=ergoStool)

ggplot(ergoStool, aes(x=Type, y=effort, colour=Type)) + geom_boxplot(size=1) + 
      facet_wrap(~Subject)


ggplot(ergoStool, aes(x=Type, color=Type, fill=Type, y=effort)) + 
      geom_dotplot(binaxis="y", stackdir="centerwhole", stackratio=0.2, 
                   position="jitter", size=4) + facet_wrap(~ Subject)

# DIFFERENCE IN MEAN EFFOR AMONG TYPE: 
# Overall, all subjects have lower effort for type 1 stool, highest effort for
# type 2 stool. Seems to be significant difference in effort for T1, T3, and T1, T2
# type stools for all subjects, also between T3, T4 and T2, T4. 
bwplot(effort ~ Type, data=ergoStool)


# Fitting the RANDOM INERCEPTS model: non-interaction
stool.lme <- lme(effort ~ Type, random = ~1|Subject, data=ergoStool)
stool.interact.lme <- lme(effort ~ Type, random = ~1|Subject/Type, data=ergoStool)

stool.lmer <- lmer(effort ~ Type + (1|Subject), data=ergoStool)
anova(stool.lmer)

# Comparing the interaction and non interactino models: 
anova(stool.lme, stool.interact.lme) # no interaction!

anova(stool.lme)
# INTERPRET: 
# Evidence of significant difference in mean effort among the stool types
# since p-value for Type < 0.0001

# coefs and p-values for fixed effects
s1 = summary(stool.lme)
s1$tTable 
# INTERPRET: 
# Significant difference in mean effort among T1,T2, and T1, T3, but not between
# T1 and T4, since p-value = 0.211 > 0.05


# Resetting to reference level  = type 3 stool
ergoStool3 <- ergoStool
ergoStool3$Type <- relevel(ergoStool3$Type, ref=3)

stool3.lme <- lme(effort ~ Type, random = ~1|Subject, data=ergoStool3)
stool3.lmer <- lmer(effort ~ Type + (1|Subject), data=ergoStool3)
summary(stool3.lme)$tTable
# INTERPRET: 
# Significant diff in mean effort between T3,T1, and T3,T2, and T3,T4. 
# Mean effort for T3 is significantly greater than for T1 (p = 0.000256, beta = -2.22 < 0)
# Mean effort for T2 is significantly higher than for T3 (p=0.0037, beta=1.667 > 0)
# Mean effort for T4 is significantly lower than for T3 (p=0.00622, beta=-1.556 < 0)



# Get the variance components
VarCorr(stool.lme)
VarCorr(stool3.lme) # same thing

# Or get full analysis by lmer
summary(stool.lmer)
summary(stool3.lmer)

# INTERPRET VARIANCE COMPONENTS: 
# --> sigma.residual = 1.100294     (within-group variation)
# --> sigma.subject = 1.332         (between-subject variation)
# So estimate of the subject variance component (between subject variation) 
# is greater than estimate of the residual variance component (within subject variation. 



# SIGNIFICANCE OF VARIANCE COMPONENTS: 
intervals(stool3.lme)
# TODO: how to test significance of variance components? 





# Estimates of random intercepts for Random Effect (subject)
cofs <- random.effects(stool.lme)
# coefficients of random effects in this no-interaction  model
# (estimates of random intercepts for subject (random effect))
cofs 



# Check model assumptions

df <- data.frame(RandResids=stool3.lme$residuals[,2],
                 FixedResids=stool3.lme$residuals[,1],
                 RandFits=stool3.lme$fitted[,2], FixedFits=stool3.lme$fitted[,1],
                 Rand=ergoStool3$Subject, Fixed=ergoStool3$Type)


# Residuals vs fitted ----------------------------------------------------------
plot(stool3.lme)
plot(stool3.lme$fitted[,2], stool3.lme$residuals[,2]) # reids vs fitted for Subject
#plot(stool3.lme$fitted[,1], stool3.lme$residuals[,1]) # reids vs fitted for fixed effect

# Residuals vs fitted for Subject
ggplot(data=df, aes(x=RandFits, y=RandResids)) + geom_point(size=2) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="red")



# QQnorm plot
qqnorm(stool3.lme)
shapiro.test(stool3.lme$residuals[,2]) # yes, no evidence to reject normality. 

ggplot(df, aes(sample = RandResids)) + 
      stat_qq(color="dodgerblue", size=4, alpha=0.7) + 
      stat_qq_line(linetype="dashed", size=1)



# Residuals vs predictors (using boxplots) ------------------------------------
plot(ergoStool3$Type, stool3.lme$residuals[,1]) # resids vs Fixed effeect (type)
plot(ergoStool3$Subject, stool3.lme$residuals[,2])
# INREPRET: all centered around zero, but no homogeneity of variance

ggplot(df, aes(x=Fixed, y=FixedResids, colour=Fixed)) + geom_boxplot(size=1)
ggplot(df, aes(x=Rand, y=RandResids, colour=Rand)) + geom_boxplot(size=1)


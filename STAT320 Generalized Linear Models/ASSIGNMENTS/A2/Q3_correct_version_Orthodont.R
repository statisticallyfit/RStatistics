setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
library(lme4)
library(lattice)

options(show.signif.stars = FALSE)
options(digits=3)

# part (a) analyze the data set

data("Orthodont")
head(Orthodont)



# Exploratory plots

## NOTE: age | Subject because Subject is the grouping factor. 
# Each Subject consists of a group of repeated measures. 
# There are replicates (repeated measures) on the Subject variable. So for 
# each block of Subject, ID_i, the  predictor age varies by its values 9,10,12,14.

## THINK: "age varies within Subject block"

## WARNING: the analysis of these data needs to account for correlation betwee
# the repeat measures to draw valid inference. 


# formula = response ~ primaryCovariate | groupingFactor
# Can write number (1) instead of primaryCovariate when no other suitable candidate.

# Orthodont is a  groupedData(distance ~ age | Subject)
plot(Orthodont, outer= ~Sex, aspect=1)

# INTERPRET: 
# 1) males have on average higher mean distance than females (since all male lines
# are averaged higher in distance than females)
# 2) male slopes seem steeper than females
# 3) male intercepts are higher. 

plot(Orthodont, outer= ~Subject, aspect=1)
# INTERPRET: shows same as above but for each individual line is plotted on its own.


# part (b) --------------------------------------------------------------------------------

# Fit a random intercepts, random slopes model with age, sex as fixed effects
# and their interaction term. 

# NOTE: must center the age variable since there is no meaningful value of 0. Here
# we start from age = 8 not age = 0 and we cannot extrapolate outside the data's domain. 
orthoData <- Orthodont
orthoData$age.centred <- Orthodont$age - mean(Orthodont$age)

# Grouping variable: Subject
# Explanatory variable: age
### Random intercepts/random slopes: requires fitting: random = ~ age | Subject
# Fixed effects: age, Sex
### Fixed part of model: required to have interaction so write: age.centred * Sex

## SOLUTIONS use age.centred even in the random part. 
ortho.lme <- lme(distance ~ age.centred * Sex, random= ~age.centred|Subject, data=orthoData)

# NOTE: the fixed effects coefs table is same as for LM model. 
summary(ortho.lme)

VarCorr(ortho.lme)
# Results: 
# sigma.intercept = 1.83
# sigma.age.centred = 0.18
# sigma.residual = 1.31

# INTERPRET: 
# sigma.age.centred (variance component of slope) is small ==> means 
# there is no big difference
# among the Subjects for the rate of growth of distance ==> equal slopes across genders ==> 
# implies random intercepts only model. 

# Also, variance for intercepts (1.83^2) is large compared to variance of slope (0.18^2)


# Testing significance of variance components: 
# cis = intervals(ortho.lme)
# cis
# cis$reStruct$Subject
# cis$sigma
intervals(ortho.lme)
# Ci for sigma.intercept = (1.337, 2.5045)
# CI for sigma.age = (0.058, 0.556)
# CI for sigma.residual = (1.08, 1.58)   # (within- subject) standard error

# INTERPRET: 

# (1) All the variance components are significantly higher than 0 since all the
# interval CI's are above 0. (Except for CI of correlation between intercept and age
# which contains zero, suggesting the correlation is not different from zero)
# This means: sigma.age (the variation in slopes) is significantly higher than 0. 
# And: sigma.intercept (variation in intercepts) is significantly higher than 0. 

# (2) CI for sigma.intercept is entirely above Ci for sigma.age so sigma.intercept
# is significantly higher than sigma.age. This means the variation in intercepts of distance
# for all genders is is significantly higher than variation in slopes of distance. 

# (3) Ci for sigma.age is all below Ci for sigma.residual so the within-subject standard
# error is significantly greater than  slope variation. 
# Ci for sigma.intercept overlaps Ci for sigma.residual so these variances are not 
# significantly different. 


# part (c) ------------------------------------------------------------------------------
# Fitting the random intercepts model with fixed effect interaction


# Grouping variable: Subject
# Explanatory variable: age
### Random intercepts only: requires fitting: random = ~ 1 | Subject
# Fixed effects: age, Sex
### Fixed part of model: required to have interaction so write: age.centred * Sex
ortho.intercept.lme <- lme(distance ~ age.centred * Sex, random= ~1|Subject, data=orthoData)

# NOTE: the fixed effects coefs table is same as for LM model. 
summary(ortho.intercept.lme)

VarCorr(ortho.intercept.lme)
# Results: 
# sigma.intercept = 1.82
# sigma.residual = 1.39


# Testing significance of variance components: 
intervals(ortho.intercept.lme)
# CI for sigma.intercept = (1.32, 2.5)
# CI for sigma.residual = (1.19, 1.62)   # (within subject standard error)

# part (d) -----------------------------------------------------------------------------

# Which model is preferrd?

anova(ortho.intercept.lme, ortho.lme)
# Going by p-value: the random slope part is not needed. 
# Going by AIC: the random intercepts model has lower AIC but the delta = 3 is small
# However, since its AIC is still smaller and it is more parsimonious, we choose the
# random-intercepts only model. 

# part (e) -------------------------------------------------------------------------------

# Check model assumptions

## Got random part residuals to be standardized but how to standardize
# the fixed model part? resid() function only gets the random part. 
df <- data.frame(SubjectResids=ortho.intercept.lme$residuals[,2], 
                 SubjectStdResids=resid(ortho.intercept.lme, type="normalized"), 
                 FixedResids=ortho.intercept.lme$residuals[,1],
                 SubjectFits=ortho.intercept.lme$fitted[,2], 
                 FixedFits = ortho.intercept.lme$fitted[,1],
                 Subject = orthoData$Subject, 
                 Sex = orthoData$Sex) #, Fixed=orthoData$age.centred)

# fits vs resids
# the Subject residuals (random part of the model)
#plot(ortho.intercept.lme, resid(., type="p") ~ fitted(.) | Sex, abline=0)

# So to get standardized residuals mus write: resid(model, type="p") or type = "normalized"
#ggplot(df, aes(x=SubjectFits, y=SubjectResids, color=Sex)) + geom_point()

# Residuals vs fitted ------------

ggplot(data=df, aes(x=SubjectFits, y=SubjectStdResids, color=Sex)) + geom_point(size=2) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
      geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
      ggtitle("Residuals vs Fitted for Grouping Factor = Subject")


# No need for fitted part ...
#ggplot(data=df, aes(x=FixedFits, y=FixedResids)) + geom_point(size=2) + 
#      geom_hline(yintercept=0, linetype="dashed", size=1,color="red") + 
#      ggtitle("Residuals vs Fitted for Fixed Line")


# Residuals vs predictors (using boxplots) ------------------------------------


# INREPRET: all centered around zero, but no homogeneity of variance

#ggplot(df, aes(x=AgeCentred, y=FixedResids, colour=AgeCentred)) + geom_point(size=2) + 
#   geom_hline(yintercept=0, linetype="dashed",color="red",size=1)
# homogeneity of variance and all centred around mean = 0

## CORRECTED: color by Gender not by Subject, which is already on x-axis!
ggplot(df, aes(x=Subject, y=SubjectStdResids, colour=Sex)) + geom_boxplot(size=1) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black")
# several outliers and no homogeneity of variance

# QQnorm plot ------------

shapiro.test(df$FixedResids) # for age
shapiro.test(df$SubjectResids) # for Subject, evidence to reject normality

#ggplot(df, aes(sample = FixedResids)) + 
#      stat_qq(color="dodgerblue", size=3, alpha=0.5) + 
#      stat_qq_line(linetype="dashed", size=1) + 
#      ggtitle("QQnorm plot for Fixed Line Residuals")

ggplot(df, aes(sample = SubjectStdResids)) + 
      stat_qq(color="dodgerblue", size=3, alpha=0.5) + 
      stat_qq_line(linetype="dashed", size=1) + 
      ggtitle("QQnorm plot for Subject Residuals")




# part (f) ------------------------------------------------------------------------------

# REVIEW of prediction interpretations: 

summary(ortho.intercept.lme)
random.effects(ortho.intercept.lme)
predict(ortho.intercept.lme, level=0:1)

line <- function(age, female) { 
      return (24.9687500 + 0.7843750*(age-mean(Orthodont$age)) -2.3210227*female -0.3048295*(age-mean(Orthodont$age))*female )
}

# level = 0: the population level (fixed effect of age) averaged over random effect (Subject?)
# level = 1: the random effect (sSubject?)

# ---> predict.fixed (level 0) = predicted response for each FIXED EFFECT (age) averaged
# over random effect (Subject). 

# mean score for age = 8 (age.centred = -3) averaged over Subject, Gender = Female
# ==> 24.96 + 0.78 * (-3) - 2.32(1) - 0.3048 * 1*(-3)
# ==> 21.209
line(8, 1)
# 21.209

# mean score for age = 10, averaged over Subject, Gender = Female
line(10, 1)
# 22.16818

# mean score for age = 12, averaged over Subject, Gender = Female
line(12, 1)
# 23.12727

# mean score for age = 14, averagd over Subject, Gender = Female
line(14, 1)
# 24.0863

# mean score for age = 8 (age.centred = -3) averaged over Subject, Gender = Male
# ==> 24.96 + 0.78 * (-3) - 2.32(0) - 0.3048 * 0*(-3)
# ==> 22.61563
line(8,0)
# 22.61563

# mean score for age = 10, averaged over Subject, Gender = Male
line(10, 0)
# 24.18438

# mean score for age = 12, averaged over Subject, Gender = Male
line(12, 0)
# 25.75313

# mean score for age = 14, averagd over Subject, Gender = Male
line(14, 0)
# 27.32188




### --> predict.RANDOM (predict.Subject) (level 1). The individual random effect (Subject)

random.effects(ortho.intercept.lme)

# mean distance for Subject=M01, with age = 8
# = mean for age = 8, avg Subject, Gender=Male + COEF for Subject=MO1
# = 22.61563 + 2.42761769
# = 25.04324


# mean distance for Subject=M01 with age = 10
# = mean for age =10, Avged Subject, Gender=Male + COEF for Subject = M01
# = 24.18438 + 2.42761769
# 26.61199

# mean distance for Subject=M01 with age = 12
# = mean for age =10, Avged Subject, Gender=Male + COEF for Subject = M01
# = 25.75313 + 2.42761769
# 28.18074

# mean distance for Subject=M01 with age = 14
# = mean for age =10, Avged Subject, Gender=Male + COEF for Subject = M01
# = 27.32188 + 2.42761769
# 29.74949

# -----
# mean distance for Subject=M02 with age = 8
# = mean for age =8, Avged Subject, Gender=Male + COEF for Subject = M02
# = 22.61563 - 1.39110677
# 21.22452
# ----
#
# mean distance for Subject = Fo8 with age = 10
# = mean for age = 10, Avged Subject, Gender = Famel + COEF for Subject = F08
line(10, 1)
# = 22.16818 + 0.63480095
# = 22.80298







# NEW DATA SET: 
new0 <- data.frame(Subject = rep(c("M11", "F03"), c(3,3)), 
                   Sex=rep(c("Male", "Female"), c(3,3)), 
                   age.centred = rep(16:18, 2) - mean(Orthodont$age))
new0
preds <- predict(ortho.intercept.lme, newdata=new0, level=0:1); preds

## FIRST ROW: Subject = M11, Age = 16 ====================================================

### --> predict.fixed
line(16, 0) # mean for subject=M11, age = 16, averaged over subject
# 28.89062

# coef for Subject = M11 
random.effects(ortho.intercept.lme)
# -1.17289394

### --> predict.Subject
# ===> mean distance for Subject = M11 with age = 16  is:
# = 28.89062 - 1.17289394
# = 27.71773


## SECOND ROW: Subject = M11, Age = 17 ====================================================

### --> predict.fixed
line(17, 0) # mean for subject=M11, age = 17, averaged over subject
# 29.675

# coef for Subject = M11 
r <- random.effects(ortho.intercept.lme)
rows = rownames(r)
r[rows == "M11", ]
# -1.17289394

### --> predict.Subject
# ===> mean distance for Subject = M11 with age = 16  is:
# = 29.675 - 1.17289394
# = 28.50211


## THIRD ROW: Subject = M11, Age = 18 ====================================================

### --> predict.fixed
line(18, 0) # mean for subject=M11, age = 16, averaged over subject
# 30.45938

# coef for Subject = M11 
random.effects(ortho.intercept.lme)
# -1.17289394

### --> predict.Subject
# ===> mean distance for Subject = M11 with age = 16  is:
# = 30.45938 - 1.17289394
# = 29.28649


## FOURTH ROW: Subject = F03, Age = 16 ====================================================

### --> predict.fixed
line(16, 1) # mean for subject=M11, age = 16, averaged over subject
# 25.04545

# coef for Subject = F03 
r <- random.effects(ortho.intercept.lme)
rows <- rownames(r)
r[rows == "F03", ]
# 0.9621202

### --> predict.Subject
# ===> mean distance for Subject = M11 with age = 16  is:
# = 25.04545 + 0.9621202
# = 26.00757


## FIFTH ROW: Subject = F03, Age = 17 ====================================================

### --> predict.fixed
line(17, 1) # mean for subject=M11, age = 17, averaged over subject
# 25.525

# coef for Subject = F03 
random.effects(ortho.intercept.lme)
# 0.9621202

### --> predict.Subject
# ===> mean distance for Subject = F03 with age = 17  is:
# = 25.525 + 0.9621202
# = 26.48712


## SIXTH ROW: Subject = F03, Age = 18 ====================================================

### --> predict.fixed
line(18, 1) # mean for subject=F03, age = 18, averaged over subject
# 26.00455

# coef for Subject = F03 
random.effects(ortho.intercept.lme)
# 0.9621202

### --> predict.Subject
# ===> mean distance for Subject = F03 with age = 18  is:
# = 26.00455 + 0.9621202
# = 26.96667
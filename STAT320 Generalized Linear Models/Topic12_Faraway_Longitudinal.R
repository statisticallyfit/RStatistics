setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
#library(lme4)
#detach(package:lme4)
library(lattice)

library(faraway)
library(dplyr)

data(psid, package="faraway")
head(psid)
psid20 <- filter(psid, person <= 20)
N <- nrow(psid)
psid$personName <- factor(paste(rep('P', N), psid$person, sep=''))

psid20 <- filter(psid, person <= 20)

ggplot(data=psid20, aes(x=year, y=income)) + geom_line(color="dodgerblue") + 
      facet_wrap(~person)

# ggplot for of xyplot: 
ggplot(data=psid20, aes(x=year, y=income + 100, group=person)) + geom_line() + 
      facet_wrap(~sex) + scale_y_log10()

# INTERPRET: 
# (i) men's incomes are higher than females (intercepts)
# (ii) mens' incomes are lesss variable than females (within-group variability???)
# (iii) mens' incomes are increasing more quickly (slopes)



### RESPONSE FEATURE ANALYSIS of slope and intercept: 


# Fitting linear model to each group (person) within the data
# A list of linear models one for each group is returned and we extract slopes
# and intercepts
detach(package:nlme)
library(lme4)
income.lmlist <- lmList(log(income) ~ I(year - 78) | person, data=psid)
intercepts <- sapply(income.lmlist, coef)[1,]
slopes <- sapply(income.lmlist, coef)[2,]

# Plotting the intercepts and slopes
df <- data.frame(intercepts, slopes, person=names(income.lmlist))

# Intercepts vs slopes scatter
ggplot(df, aes(x=intercepts, y=slopes)) + geom_point() + ggtitle("Scatter of intercepts vs slopes")
# boxplot of intercepts
pgender <- psid$sex[match(1:85, psid$person)]
boxplot(split(slopes, pgender))
#INTERPRET: 
# income growth rates are higher and more variable for women than men. 
#ggplot(df, aes(x=person, y=intercepts))

# Testing difference in income growth rates (slopes) between men and women: 
t.test(slopes[pgender=="M"], slopes[pgender=="F"])
# ===> significantly higher slope for females since t < 0 
# ===> women have significantly higher growth rate than men. 

# Testing intercepts (at year = 1978)
t.test(intercepts[pgender=="M"], intercepts[pgender=="F"])
 # ==> males have significantly higher intercepts. 



### MODEL: 
# Fixed part: the part of the model where income is predicted by subject's age, 
# gender and educaion. 
# Random part: variation other than predictors that affect a subject's income. 
# these may cause income to be higher or lower (random intercepts) or may cause
# the income to grow at a faster or slower rate (random slopes). 
# This variation can be modeled with a random intercept and slope, respectively, for
# each subject. 
# Also can expect some year-to-year variation WITHIN each subject (residual variance)
# Assumption: this error is homogeneous and uncorrelated. 

psid$year.centred <- psid$year - mean(psid$year)
income.lmer <- lmer(log(income) ~ year.centred * sex + age + educ + (year.centred|person), 
                    data=psid)

library(nlme)
# Random effect(grouping factor) = person
# Primary covariate = year.centred

# formula = random= ~ primaryCovariate | groupingFactor (the random effect)
# Can write number (1) instead of primaryCovariate when no other suitable candidate.

income.lme <-lme(log(income) ~ year.centred*sex + age + educ, 
                 random= ~ year.centred|person, data=psid)
summary(income.lmer)
summary(income.lme)


# INTERPRET: 

### FIXED EFFECTS: 

## line: Y = B0 + B1*yearC + B2*sexM + B3*age + B4*educ + B5*yearC*sexM
# female line:  Y = B0 + B1*yearC + B3*age + B4*educ
#               Y = 6.72 + 0.085*yearC + 0.011*age + 0.104*educ
# male line:    Y = (B0 + B2) + (B1 + B5)*yearC + B3*age + B4*educ
#               Y = 7.86052 + 0.059*yearC + 0.011*age + 0.104*educ

## EDUCATION: income increases about 11% for every additional year of eduction. 
## AGE: for every additional age increase, income increases about 10.9% (not significant)
## YEAR.C: for females, income increases about 8.5% each year, and for men 
# it increases about 5.9% each year


### RANDOM EFFECTS: 

VarCorr(income.lme)
# sigma.intercept = 0.537
# sigma.year.cnt = sigma.slope = 0.0489
# sigma.residual = 0.68357

# Overall: sigma.residual (year-to-year within-subject variation) is larger than 
# intercept and slope variation. Smallest variation in slope. 

# DETAIL: 
## sigma.slope vs. sigma.intercept: variation in increase in income is relatively 
# small (0.04) compared to variation in overall income between individuals (0.537)
# But within-group variation (residual variation, or variation between individuals) 
# is 0.68 is quite larger than both. 
intervals(income.lme)


# TESTING FIXED EFFECTS:  (or can just use the lme summary)
library(pbkrtest)
income.r.lmer <- lmer(log(income) ~ year.centred * sex + age + educ + 
                            (year.centred|person), data=psid, REML=FALSE)
income.nointeraction.lmer <- lmer(log(income) ~ year.centred + sex + age + educ + 
                                        (year.centred|person), data=psid, REML=FALSE)
KRmodcomp(income.r.lmer, income.nointeraction.lmer)

# INTERPRET: 
# The interaction term is significant since p-value = 0.03 so there is a significant
# difference in slopes and thus females have a higher increase rate in income than males. 


# TESTING RANDOM EFFECTS: 
randCI.boot <- confint(income.r.lmer, method="boot")
# INTERPRET: 
# All standard deviations are well above zero.




### DIAGNOSTICS: 

# QQnorm: can break down residuals by gender: 
diagd <- fortify(income.r.lmer)
ggplot(diagd, aes(sample = .resid)) + stat_qq() + facet_grid(~sex)
# Not normal, and greater variance in female incomes. 

# Residuals vs. fitted: 
diagd$edulevel <- cut(psid$educ, c(0, 8.5, 12.5, 20), labels=c("lessHS", "HS", "moreHS"))
ggplot(diagd, aes(x = .fitted, y=.resid)) + geom_point(alpha=0.3) + 
      geom_hline(yintercept=0) + facet_grid(~edulevel) + xlab("Fitted") + ylab("Residuals")
# INTERPRET: 
# need different response transformation, since have increasing variance. 





# ------------------------------------------------------------------------------------

# REPEATED MEASURES: 

# Response = lag in miliseconds between light flash and response in eye cortex.
# Each eye is tested at four different lens powers. 

data(vision, package="faraway")
vision


# Exploratory plot: see how acuity changes with increasing power: 
N = nrow(vision)
vision$npower <- rep(1:4, times=14) #factor(paste(rep('N', N), rep(1:4, times=14), sep=''))
vision$subject <- factor(paste(rep('S', N), vision$subject, sep=''))
vision

# INTERPRET: 
# No trend or difference between right and left eyes. 
# But individual #6 appears anomalous with large difference between eyes. 
ggplot(data=vision, aes(y=acuity, x=npower, linetype=eye)) + geom_line() + 
      facet_wrap(~subject, ncol=4) + 
      scale_x_continuous("Power", breaks=1:4, labels=c("6/6", "6/18", "6/36", "6/60"))

#Above they create a fake label for Power since the below doesn't work: 
#ggplot(data=vision, aes(x=power, y=acuity, linetype=eye )) + geom_line()  + 
#      facet_wrap(~subject, ncol=4)



## MODEL: 

# Fixed effect: power (6/60, 6/3 ...)
# Random effects (grouping factor) = subject
# Nested: eye (within subjects) since we do not believe there is any consistent
# right-left eye difference between individuals, so assume eye is randomly sampled
# iwthin subjects. 
#### NOTE: if there WAS a cnonsistent left vs. right eye effect then would not include
# the nested eye-in-subject term and so model would be: 
# acuity ~ power + (1|subject) + (1|eye)

bwplot(acuity ~ eye | subject, data=vision)

eye.nested.lmer <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), data=vision)
# same as
eye.nested.lme <- lme(acuity ~ power , random = ~1|subject/eye, data=vision)

summary(eye.nested.lmer)
summary(eye.nested.lme)


## TESTING FIXED EFFECTS (power effect):
eye.r.lmer <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), vision, REML=F)
eye.nopower.r.lmer <- lmer(acuity ~ 1 + (1|subject) + (1|subject:eye), vision, REML=F)
KRmodcomp(eye.r.lmer, eye.nopower.r.lmer)

# Marginally significant power effect> So there is some trend in acuity with power
# but the estimated effects do not fit the trend. 
# Acuity is greatest at highest powr 6/60 bu lowest for the second highest power 6/36. 

# Removing the outlier (sixth subject) at the power 6/60: 
eye.r.lmer <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), vision, REML=F, 
                   subset=-43)
eye.nopower.r.lmer <- lmer(acuity ~ 1 + (1|subject) + (1|subject:eye), vision, REML=F,
                           subset=-43)
KRmodcomp(eye.r.lmer, eye.nopower.r.lmer)
# Now the power effect is significant. But it appears this is because of the effect
# at the highest power only. 

# Use Helmert contrasts to check that the highest power has a higher acuity than
# average of the first three levels: 
op <- options(contrasts = c("contr.helmert", "contr.poly"))
eye.r.contrasts.lmer <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), vision, REML=F,
                   subset=-43)
eye.contrasts.lme <- lme(acuity ~ power , random = ~1|subject/eye, data=vision,subset=-43)

summary(eye.r.contrasts.lmer)
summary(eye.contrasts.lme)

# INTERPRET: Only the third contrast is significant (p=0.0028 < 0.05)

contr.helmert(4)
# Third contrast (column) represents difference between the average of the first three
# levels and the fourth level. 

# switch back to default contrasts: 
options(op)



### DIAGNOSTICS: 
#Resids vs fitted
plot(resid(eye.r.contrasts.lmer) ~ fitted(eye.r.contrasts.lmer))
# QQnorm for the random effects for the eyes. 
qqnorm(ranef(eye.r.contrasts.lmer)$"subject:eye"[[1]])



# ----------------------------------------------------------------------------------
# 11.3 Multilevel
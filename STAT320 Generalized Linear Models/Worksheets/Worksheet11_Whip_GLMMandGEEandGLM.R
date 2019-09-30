setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)

library(lme4) # for glmer() for fitting GLMM model
library(nlme)
#detach(package:nlme)
library(lattice)
library(car) # for Anova() for testing glmer
library(MASS) # for glmmPQL
library(geepack)
library(glmmML) # for glmmML

options(show.signif.stars = FALSE)


# Response: eTot = energy scores
# Subejcts: Subj 
# Group: whiplash subjects (W) or control subjects (N)
# Test variable: the tests that each subject underwent, standing on a balance plate. 
# Levels: 
# - teaoh: eyes open on hard surface
# - tech: eyes clsoed on hard surface
# - tvch: visual conflict hard surface, where helmet placed on subject
# - teof: eyes open on foatm (soft) surface
# - tecf: eyes closed on foam (soft) surface
# - tvcf: visual conflict foam (soft) surface, where helmet placed on subject. 
whiplash <- read.table("data/whip.txt", header=TRUE)
head(whiplash)
whiplash$Subj
whiplash$Test

whipData <- whiplash[, c(1,12, 2,3,11)]
head(whipData, 10)



# part a) Density plots ----------------------------------------------------------------

# Density plots of total energy (response) by treatment combo (Group x Test)
energy.eda <- densityplot(~ eTot | Test * Group, data=whipData, xlim=c(0, 6e3), 
                          ylim = c(0, 0.002))
plot(energy.eda)

ggplot(whipData, aes(x=eTot)) + geom_density(color="dodgerblue", size=1) + xlim(c(0, 6000)) + 
      facet_wrap(~ Group + Test, ncol=6)
# INTERPRET: distributions of response variable total energy (eTot) are all positively
# skewed, consistent with the gamma distribution (so use GLMM with gamma error distribution)

# Boxplots
energy.edab <- bwplot(~ eTot | Test*Group, data=whipData)
plot(energy.edab)

ggplot(whipData, aes(x=Group, y=eTot, color=Group)) + 
      geom_boxplot(size=1) + ylim(c(0, 5000)) + 
      facet_wrap(~ Group + Test, ncol=6)
# INTERPRET: again, distributions are right skewed (long right tail tending towards infinity)
# with most of data values (energy scores) clustered at low values of energy. 



# part b) -----------------------------------------------------------------------------------

# Grouped data object, plots of response for each subject across the 6 Tests by group
# NOTE: using test as quantitative allows you to visualize the data. 

whipGroupedData <- groupedData(eTot ~ test | Subj, outer = ~Group, data=whipData)
head(whipGroupedData, 10)

# Plot for Subjects (x = Test, y = eTot) by Group (facet)
plot(whipGroupedData, outer=TRUE, aspect=1)
plot(whipGroupedData, outer=TRUE, aspect=1, ylim=c(1,30000))

# Plot for each subject (x = Test, y = eTot)
plot(whipGroupedData, aspect=1) # plot for each subject
# INTERPRET: not much within-subject slope variation ===> no random slopes
# not much within-subject intercept variation either ===> no random intercepts?

# NOTE: seems arbitrary that we say Group is the facetting variable and test is on x-axis
# since we can do either way, but it is just that there are too many tests as opposed to 
# groups so easier to plot with Groups as facet. 


# PLOT 1 =====================================================================================
# WITHIN-SUBJECT VARIATION PLOTS (random effects)


numSubjects <- length(levels(whipData$Subj))

# y = eTot, x = test, group (facet) = Group
ggplot(data=whipData, aes(x=test, y=eTot, color=Subj)) + geom_line() + geom_point() +
      facet_wrap(~  Group) +
      scale_colour_manual(values=sample(ggplotColors(numSubjects)))

# teoh = 1   (hard)
# tech = 2   (hard)
# tvch = 3   (hard)
# teof = 4   (soft)
# tecf = 5   (soft)
# tvcf = 6   (soft)
ggplot(data=whipData, aes(x=test, y=eTot, color=Subj)) + geom_line() + geom_point() +
      facet_wrap(~ Group, ncol=2) + ylim(c(0,6500)) +
      scale_colour_manual(values=sample(ggplotColors(numSubjects)))

# INTERPRET: 
# -- Variability among slopes and intercepts of whiplash subjects ===> random slopes and intercepts
# -- First three tests (all on hard surface) have small variability among ALL subjects
# compared to the soft tests, for both groups (N, W)
# So: variability in eTot within subjects for tests on the hard surface is small compared 
# to variability among subjects for tests on the foam surface. 
# ====> random slopes

# -- Little variability among subjects in control (N) group for HARD tests with increased
# variability among subjects for the SOFT tests. 

# Outlier: subject jk in the W group, is outlier in nearly all tests. 
# Subject sg has high response for tests 6 in (N) group

# Random intercepts: seems to be variability between intercepts for the (W) group ===> 
# suggests random intercepts for the whole model (?)



# Plot for each subject individually: 
ggplot(data=whipData, aes(x = test, y = eTot)) + geom_line() + geom_point() + 
      facet_wrap(~Subj)

# INTERPRET: this plot helps to identify unusual subjects (sg in N and most of the W
# subjects)

# PLOT 2: =====================================================================================

# Response Feature Analysis (comparing slopes / intercepts with boxplots and CI's)
'
detach(package:lme4)
library(nlme)

# Random slopes = test (quant)
whip.lmlist <- lmList(eTot ~ test | Subj, data=whipData)
whip.ci <- intervals(whip.lmlist) 
#plot(blues.ci)

# Plotting the intercept and slope Confidence intervals (95%)
intDf <- data.frame(whip.ci[,,1]) # drink.ci[,,"(Intercept)"])
slopeDf <- data.frame(whip.ci[,,2]) # drink.ci[,,"weeks"])
subjectNames <- rownames(intDf) # same for int and slope dfs

ptest =  whipData$test[match(levels(whipData$Subj), whipData$Subj)]

ciDf <- data.frame(Estimate=c(intDf$est., slopeDf$est.), 
                   Lower = c(intDf$lower, slopeDf$lower),
                   Upper = c(intDf$upper, slopeDf$upper),
                   Type=c(rep("intercept", nrow(intDf)), rep("slope", nrow(slopeDf))),
                   Subject= subjectNames, 
                   treatment = ptest) # TODO why not working

ggplot(ciDf[ciDf$Type == "intercept",], aes(x=Estimate, y=Subject, color=treatment)) + 
      geom_errorbarh(aes(xmin=Lower, xmax=Upper)) + 
      geom_line() + geom_point() + xlab("Intercept")
ggplot(ciDf[ciDf$Type == "slope",], aes(x=Estimate, y=Subject, color=treatment)) + 
      geom_errorbarh(aes(xmin=Lower, xmax=Upper)) + 
      geom_line() + geom_point() + xlab("Slope") 
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


# (2) VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# ----- Random slopes: (Slopes within treat): the rate of increase in depression over time
# (slope) differs for subjects within a treatment (can see this more clearly than on lines plot)
# For Blues, there is less variation in slopes among subjects than for TAU since boxplot
# for Blues is narrower than for TAU. 

# ====> suggests need for random slopes: ~ Months | Subject




intByTreatDf <- melt(split(intDf$est., ptreat))
colnames(intByTreatDf) <- c("intercept", "treat")
ggplot(intByTreatDf, aes(x=treat, y=intercept, color=treat)) + geom_boxplot(size=1)

# (1) VARIABILITY AMONGST GROUPS (fixed effect )
### --- Intercepts among treatments:
# Blues boxplot is lower than TAU boxplot, so can see that intercepts for Blues group
# are generally lower. Not significant since boxplots overlap. 


# (2) VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

# ----- Random Intercepts: (Space between lines within treat): 
# Within each treatment group there is variability among subjects since the 
# lines are spaced out. 
# Variation in intercepts among subjects is similar for Blues and TAU group since each of
# their boxplots have similar width. 

# =====> cannot say if the width is wide enough to justify need for random intercepts model: 
# ~ 1|subject

'


# PLOT 3: =====================================================================================

# BETWEEN-GROUP VARIATION PLOTS (fixed effects, acvering over subjects (individual variation)) 

interactionPlot(data=whipData, x.factor="Test", trace.factor = "Group", response="eTot")
# Different slopes and interceps. 
# Different slopes ===> fixed interaction between Group * Test
# INTERPRET: 
# - the energy expenditure goes down from soft to hard surface. 
#  - the increase in mean energy expenditure moving from hard to soft sorface is the same
# for the control group, regardless of eyes open, closed, or helmeted. 
# - energy expenditure for W group is much larger for the test on foam with eyes closed
# - overall, energy is higher for W group than the N group




# part c) Fitting the model -------------------------------------------------------------------

# NOTE: doesn't make sense to consider Random Slopes model if (test) is purely qualitative. 
# But if you argue tests are ordered in some way, then we can consider random slopes model
# with test as quantitative than qualitative (Test)


# Fit GLM model (assumes independence among experimental units (Subject)) --------------------
whip.glm <- glm(eTot ~ Test * Group, family=Gamma(link=log), data=whipData)
anova(whip.glm, test="Chisq")
# INTERPRET: interaction not significant. 

summary(whip.glm)


## Fit GEE model (allows for correlation among tests results of subject) ----------------------
whip.gee <- geeglm(eTot ~ Test * Group, id = Subj, family=Gamma(link=log), data=whipData, 
                   corstr="exchangeable")
summary(whip.gee)
anova(whip.gee) # once errors have been explained, the interaction is significant, even without random slopes

ggAcf(whip.gee$residuals, lag.max = 50)
# there is sinusoidal pattern - not good, also some outliers. 



## Fit GLMM model (random intercepts and slopes) -----------------------------------------------
#whip.glmm <- glmer(eTot ~ Test * Group + ())

# Random intercepts (Subj) accounts for individual within-subject variation. 
whip.glmmPQL <- glmmPQL(eTot ~ Test*Group, random = ~1|Subj,  
                        family=Gamma(link=log), data=whipData)
Anova(whip.glmmPQL)
summary(whip.glmmPQL)      


# GLMM using glmer
library(lme4)
whip.glmer <- glmer(eTot ~ Test*Group + (1|Subj), family=Gamma(link=log), data=whipData,
                    control=glmerControl(optimizer="bobyqa",check.conv.grad=.makeCC("warning",1e-3) ))

Anova(whip.glmer)


# Fit non-interaction model to compare
whip.nointeract.glmer <- glmer(eTot ~ Test + Group + (1|Subj), family=Gamma(link=log), data=whipData)

anova(whip.nointeract.glmer, whip.glmer)
# yes interaction term is singificant, so keep interaction model



# NOTE: in assignments, produce summary table of preferred model only at the end, after
# finding the preferred model. 
summary(whip.glmer)
# INTERPRET: 
# -- Test * Group term: 
# ---> Testteof:GroupW: (p-value = 0.02, coef = -0.56): so mean energy expenditure is 
# significantly higher for the control Test (tecf) than for Test=teof , among the W and N groups
# (coef = (mu_TEOF,W - mu_TECF,W) - (mu_TEOF,N - mu_TECF, N))

# --> GroupW: (p-value = 0.000011, coef = 1.37): so mean energy expenditure is significantly
# higher for the whiplash group than for the control group, averaging over Test levels
# ***** averaging over Test is not allowed since Test * Group is significant. 

# --> Testtech: (p-value < 2e-16, coef = -1.6778): so mean energy expenditure is significantly
# lower for tech Test  than for control test (tecf), and this is for the control Group (N)


# Residuals plots ------------------------------------------------------------------------------

# fixedResids = whip.glmer$residuals[,1]
df <- data.frame(Residuals = residuals(whip.glmer, type="pearson"), 
                 Fitted = fitted(whip.glmer), Test = whipData$Test, Group = whipData$Group,
                 Subject = whipData$Subj)


# (1) Residuals vs fitted --------------------------------------------------------------------

ggplot(df, aes(x=Fitted, y=Residuals, colour=Test)) + geom_point(size=2) +
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
      geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
      facet_wrap(~Group + Test)
# All within group combos are generally normal and homoskedastic about y = 0
# No outliers (dots beyond the dashed lines)

ggplot(df, aes(x=Fitted, y=Residuals, colour=Group)) + geom_point(size=2) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
      geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
      facet_wrap(~Group)
# All residuals are generally normal and homoeskedastic


# (2) QQPLOT ---------------------------------------------------------------------------------

# (3) Boxplot of predictors vs residuals (by Subject) -----------------------------------------

ggplot(df, aes(x=Subject, y=Residuals, colour=Group)) + geom_boxplot(size=1)  +
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
      geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") 

# INTERPRET: no outliers and most of the boxplots show little right skew so gamma was
# effective in taking structure out of the data, all nearly centered at mean = 0
# But differing variance (IQR) among boxplots so heteroskedasticity is present (cosntant
# variance assumption is violated)


# Shapiro Test
shapiro.test(df$Residuals) # EW! very non normal. 


# Question: 
# coef(whip.glmer) # are these the lines for each of the subjects?

# Density plot of random subject effects (dashed density) versus the theoretical normal 
# distribution (solid line)
#U <- sort(as.numeric(whip.glmer$coefficients$random$Subj))

VarCorr(whip.glmer)
m = as.matrix(as.data.frame(VarCorr(whip.glmer)))
sigma.interceptSubj <- as.numeric(m[2,5])
sigma.residual <- as.numeric(m[1,5])

#plot(density(whip.glmer@u, bw=0.4))
#lines(whip.glmer@u, dnorm(mean = 0, sd=sigma.interceptSubj, x = sort(whip.glmer@u)), lty=2)
#su = sort(whip.glmer@u)
#lines(su, dnorm(x=su, mean=0, sd=sigma.interceptSubj))

      
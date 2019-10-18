setwd("C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/stat320_rcode")
#source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/FORMULAS.R')
#source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/PLOTTING.R')
source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/Rfunctions.R')


library(ggfortify)
library(lme4)
library(forecast)
library(lattice)
# library(nlme)

options(show.signif.stars = FALSE)

# Time within Tree within Treatment
# Responses are linear in time ==> can use lmer (not glmer)

# Subject = Tree
# Random slope = Time
# Random intercept = Treatment

treeData <- read.table("data/trees.txt", header=TRUE)
head(treeData, 50)

N <- nrow(treeData)
treeData$Treatment <- factor(paste0(rep("Treat", N), treeData$Treatment, sep=''))

# Make tree a factor
treeData$Tree <- factor(paste(rep("T", N), treeData$Tree, sep=''))

# part a) ------------------------------------------------------------------------------

# Exploratory plots

# PLOT 1: =============================================================================
# Usable for identifying (1) between-group variation, and (2) within-group variation

# formula = response ~ primaryCovariate | groupingFactor or ~1 |groupingFactor when 
# no other suitable candidate
treeData.grouped <- groupedData(resp ~ Time | Tree, outer= ~Treatment, 
                                data=treeData)

numSubjects <- length(levels(treeData$Tree))
numSubjects



ggplot(data=treeData, aes(x=Time, y=resp, color=Tree)) + geom_line(size=1) + geom_point() +
  facet_wrap(~ Treatment, ncol=4 ) + 
  scale_colour_manual(values=sample(ggplotColors(numSubjects)))


######## (1) Addressing: VARIABILITY AMONGST GROUPS: (fixed effect interaction)

# TREAT * TIME (fixed effect)

### INTERCEPTS among treatments over time:
# ---> Similar intercepts among  all treatments. 
### SLOPES among treatments over time:
# ---> Fanning out slopes for Treat=A, and steeper slopes for Treat=D than for A, B. 
# ====> SUGGESTS very week TREAT*TIME interaction. 



######## (2) Addressing: 
#   VARIABILITY AMONG INDIVIDUALS WITHIN A GROUP: (random intercepts/ random slopes)

### RANDOM INTERCEPTS: (Space between lines within treat): 
# Within each treatment group there is variability among subjects (trees) since the 
# lines are spaced out. Generally more spacing in the Treat = A, D, B groups. 
# =====> suggests some need for random intercepts model: ~ 1|Tree

### RANDOM SLOPES: (Slopes within treat): the rate of increase in resp over time
# (slope) is similar for all Trees across treatment groups but steeper
# for C, D, and some lower observations for A, B. 
# Not big difference in slopes ==> weak need for random slopes: ~ Time | Tree




# PLOT 2: =====================================================================================
# Usable for identifying (1) between-subject variation, and (2) within-subject variation

### INTERCEPTS AND SLOPES 95% CI: 

#### Can visualize the mean response as a weighted average of the individual Subject profiles
# provided that the between-subject variance is included in the averaging. 
# Finding linear response for each Subject using lmList()
# Can help explain random effects when the model is fitted. 

detach(package:lme4)
detach(package:nlme)
library(nlme)

tree.lmlist <- lmList(resp ~ Time | Tree, data=treeData)
tree.lmlist

# note: must load only nlme never lme4 at the same time. 
tree.lmlist <- intervals(tree.lmlist) 

# Plotting the intercept and slope Confidence intervals (95%)
intDf <- data.frame(tree.lmlist[,,1]) 
slopeDf <- data.frame(tree.lmlist[,,2]) 
subjectNames <- rownames(intDf) # same for int and slope dfs

ptreat = treeData$Treatment[match(levels(treeData$Tree), treeData$Tree)]

ciDf <- data.frame(Estimate=c(intDf$est., slopeDf$est.), 
                   Lower = c(intDf$lower, slopeDf$lower),
                   Upper = c(intDf$upper, slopeDf$upper),
                   Type=c(rep("intercept", nrow(intDf)), rep("slope", nrow(slopeDf))),
                   Subject= subjectNames, 
                   treatment = ptreat)

ggplot(ciDf[ciDf$Type == "intercept",], aes(x=Estimate, y=Subject, color=treatment)) + 
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), size=1) + 
  geom_line() + geom_point(size=2) + xlab("Intercept")

ggplot(ciDf[ciDf$Type == "slope",], aes(x=Estimate, y=Subject, color=treatment)) + 
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), size=1) + 
  geom_line() + geom_point(size=2) + xlab("Slope") 

#INTERPRET: 
# Intercepts: the intercepts vary BETWEEN subjects (trees) 
# so suggests random intercepts model

# Slopes = lots of slope variation BETWEEN subjects so suggests random slopes model
# ===> should fit: randintercepts/random slopes model





## PLOT 3: ===================================================================================
# usable for identifying (1) between-group(treat) variation (so just fixed effect interaction)

# Interaction plot of week and treatment: 
interactionPlot(data=treeData, x.factor="Time", trace.factor="Treatment", response="resp")

# INTERPRET: suggests no fixed effect interaction between Treat * Time
# ## Intercepts for treat C, D are similar and higher than the lower intercepts
# of treat A, B
### Slopes are generally similar with C, D being slightly steeper than for A, B

### Overall no strong interaction between Time * Treatment. 



# part b) -------------------------------------------------------------------------------------

# List and plot the linear response for each subject (Tree)


tree.xyp <- xyplot(resp ~ Time, data=treeData.grouped, groups=Tree, 
                   panel=panel.superpose, panel.groups = panel.lmline)
tree.xyp
# INTERPRET: 
# The xyplot shows the linear fits of each Tree for resp ~ Time.  
# Can see some slopes differ (so rate of resp per Tree differs) and 
# intercepts are similar. Evidence of fanning out: slopes increase at different rates so the
# distance between Tree resp values changes over time. 


# part c) -------------------------------------------------------------------------------

### LME Model: 


# Fixed part: Treatment, Time
# Random part: variation other than predictors that affect a subject's resp .
# These may cause resp to be higher or lower (random intercepts) or may cause
# the Score to grow at a faster or slower rate (random slopes). 
# This variation can be modeled with a random intercept and slope, respectively, for
# each subject (tree). 
# Also can expect some time variation WITHIN each subject (residual variance)
# Assumption: this error is homogeneous and uncorrelated. 

# NOTE: must fit using method="ML" to compare models with different fixed effects
# but same random effects.
# Results for final model should be found using REML. 

# STEP 1: obtain the random effects part of the model: ========================================

# Getting the random structure first: 
tree.slopes.lme <- lme(resp ~ Treatment*Time, random= ~Time|Tree, data=treeData)

tree.intercepts.lme <- lme(resp ~ Treatment*Time, random= ~1|Tree, data=treeData)

anova(tree.intercepts.lme, tree.slopes.lme) # rand slopes is significant. 

# INTERPRET: 
# Random slopes is significant so the Subjects change at different rates over Time.  

# Confirming with variance components: 
VarCorr(tree.slopes.lme)

# var.time(slope) = 2.13
# var.intercept = 15.704
# var.residual = 13.9

### Within-subject variation (residual) is similar to intercept variation, so justifies
# the need for random intercepts model. 
### Variation among slopes is 2.13 is small relative to intercept variation so higher
# variatin in intercepts than among slopes. 



# Significance of variance components: 
intervals(tree.slopes.lme)

# INTERPRET: 
# (1) sigma.slope and sigma.intercept are not overlapping; sigma.intercept is 
# significantly higher than sigma.slope ==> confirms random intercepts needed. 
# (2) residual sigma is overlapping with sigma.intercept
# (3) residual sigma is significantly higher than sigma.slope



### STEP 2: get the fixed part of the model

#### Comparing fixed effects for random intercepts model: we must refit the intercepts model
# using ML to be able to compare models with different fixed effects but same random effects.
tree.slopes.ML.lme <- lme(resp ~ Treatment*Time, random = ~Time|Tree, data=treeData,method="ML")
tree.slopes.ML.maineffects.lme <- lme(resp ~ Treatment + Time, random = ~Time|Tree, data=treeData, method="ML")

anova(tree.slopes.ML.maineffects.lme, tree.slopes.ML.lme)
# OR
anova(tree.slopes.ML.lme) # shows only fixed effects (between group variation)
# INTERPRET: 2-way interaction is significant so keep it


# FOUND THE FINAL MODEL: 
# Refitting model using method = REML
tree.final.lme <- lme(resp ~ Treatment*Time, random = ~Time|Tree, data=treeData)
anova(tree.final.lme)



# part d)--------------------------------------------------------------------------------------------

#Summarize findings
summary(tree.final.lme)

random.effects(tree.final.lme)

# ---> Treat * Time is significant

# ---> TreatB:Time: coef=1.166 > 0, p=0.18 ==> change in resp over time is NOT significantly higher for
# trees on treatB than on treatA. 

# ---> TreatC:Time: coef = 2.6377 > 0, p=0.00288 ==> change in resp over time is significantly higher for
# trees on treatC than on treatA. 

# interpret treatD:time same way

# ---> do the lines analysis and compare intercepts: 
# ----> intercepts significantly different for treatD vs A only. 




### RESIDUALS for LME

fixedResids = tree.final.lme$residuals[,1]
df <- data.frame(SubjectResids = tree.final.lme$residuals[,2], 
                 SubjectStdResids = resid(tree.final.lme, type="normalized"), 
                 FixedResids = tree.final.lme$residuals[,1],
                 FixedStdResids = (fixedResids - mean(fixedResids))/sd(fixedResids), 
                 SubjectFitted = tree.final.lme$fitted[,2], 
                 FixedFitted = tree.final.lme$fitted[,1],
                 Subject = treeData$Tree, 
                 treatment = treeData$Treatment)

###### Residuals vs fitted ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# (1) Residuals vs fitted (by Subject (random part))
ggplot(data=df, aes(x=SubjectFitted, y=SubjectStdResids, color=treatment)) + 
  geom_point(size=2) + 
  geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
  geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") + 
  facet_wrap(~treatment)+
  ggtitle("Residuals vs Fitted (Grouping Factor = subject)")


# (2) Residuals vs fitted (by Age (systematic or fixed part))

ggplot(data=df, aes(x=FixedFitted, y=FixedStdResids, color=treatment)) + geom_point(size=2) + 
  geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
  facet_wrap(~treatment) +
  ggtitle("Residuals vs Fitted for Fixed Line")


######  QQ plot of residuals (fitted) ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

shapiro.test(df$FixedResids) # for fixed part of the model,  normal
shapiro.test(df$SubjectResids) # for subject, normality

# (1) Standardized residuals by treat (Fixed part): 
ggplot(df, aes(sample = FixedStdResids)) + 
  stat_qq(color="dodgerblue", size=3, alpha=0.5) + 
  stat_qq_line(linetype="dashed", size=1) + 
  ggtitle("QQnorm plot for Fixed Model")

# (2) Standardized residuals by Subject (random part): 
ggplot(df, aes(sample = SubjectStdResids)) + 
  stat_qq(color="dodgerblue", size=2) + 
  stat_qq_line(linetype="dashed", size=1) + 
  ggtitle("QQnorm plot for Subject")

### Residuals vs predictors (using boxplots) +++++++++++++++++++++++++++++++++++++++++++++++++++

# (1)  Boxplot of residual vs predictor (by treat (fixed part))
ggplot(df, aes(x=treatment, y=FixedStdResids, colour=treatment)) + geom_boxplot(size=1) + 
  geom_hline(yintercept=0, linetype="dashed",color="black",size=1)
# homogeneous error variance, some outliers, centered around mean = 0

# (2) Boxplot of residual vs predictor (by subject (random part))

ggplot(df, aes(x=Subject, y=SubjectStdResids, colour=treatment)) + geom_boxplot(size=1)  +
  geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
  ggtitle("Subject vs Standardized Subject Residuals")
# non-homogeneity of variance, with outliers, and general non-centering around y = 0 line. 



## ACF plot

ggAcf(tree.final.lme$residuals[,2]) 


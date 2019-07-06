setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/DATA.R')

options(digits=10, show.signif.stars = FALSE)


# online resource for setting orthogonal contrasts
# https://psychstatsworkshop.wordpress.com/2016/05/18/quickly-find-orthogonal-contrasts-for-any-number-of-conditions/
# https://rstudio-pubs-static.s3.amazonaws.com/65059_586f394d8eb84f84b1baaf56ffb6b47f.html



melonData <- data.frame(Yield=c(25.12, 17.25, 26.42, 16.08, 22.15, 15.92,
                                40.25, 35.25, 31.98, 36.52, 43.32, 37.10,
                                18.3, 22.6, 25.9, 15.05, 11.42, 23.68,
                                28.55, 28.05, 33.2, 31.68, 30.32, 27.58),
                        Variety=c(rep("A",6),rep("B",6),rep("C",6),rep("D",6)))
melonData

# Getting the means (according to variety type): 
with(melonData, tapply(Yield, INDEX=list(Variety), mean))




melon.lm <- lm(Yield ~ Variety, data=melonData, x=TRUE)
summary(melon.lm)
anova(melon.lm)
# so significant difference of mean yield across different varieties? 
betaCI(melon.lm)
# INTERPRET: 
# --- B and D are both significantly greater mean yield than A. 
# --- C mean yield is not significantly different than mean yield of A. 



# -----------------------------------------------------------------------------
# original contrasts
melon.lm$x
contrasts(melonData$Variety)

# new contrasts
ACvBD <- C(melonData$Variety, contr=c(1,-1,1,-1), how.many=1)
AvC <- C(melonData$Variety, contr=c(1, 0, -1, 0), 1)
BvD <- C(melonData$Variety, c(0,1,0,-1), 1)

# check they are orthogonal: 
mat <- matrix(c(1,-1,1,-1, 1,0,-1,0,  0,1,0,-1), ncol=3)
rownames(mat) <- c("A", "B", "C", "D")
colnames(mat) <- c("ACvBD", "AvC", "BvD")
#  can check with transpose
t(mat) %*% mat


# refit the model with the orthogonal contrasts to partition variety SS
melon.orthog.lm <- lm(Yield ~ ACvBD + AvC + BvD, data=melonData, x=TRUE)
betaCI(melon.orthog.lm)
# INTERPRET: 
# -- rotstock 1 diff with rootstock 2 is significant: 
# ---- since B,D are negatives, the negative diff in acvbd1 means
# that BD are higher than AC so rootstock2 makes highe rmean yield. 
# --- 2) AvC not signiificant (in graph too)
# --- 3) BvD is significant = means B makes significantly higher mean yield than D. 


# overall mean is the intercept of contrasts
# Add any means that are in contrasts (source: summary table)
summary(melon.orthog.lm)


# MANUALLY 
# mu = 26.8
# mu_A = 26.8 + -6.8 + 0.5 = 20.5
# mu_B = 26.8 - (-6.8)  + 3.8 = 37.4
### here ACvBD the B is negative so subtract its mean contrast thing.

# mu_C = 26.8 + -6.8 - (0.5) = 19.5
# here in AvC the C has negative so subtract its mean 0.5

# mu_D = 26.8 - (-6.8) - (3.8) = 29.8


# BETTER WAY: 
mat.intercept <- cbind(c(1,1,1,1), mat)
colnames(mat.intercept) <- c("Intercept", "ACvBD", "AvC", "BvD")
mat.intercept
cofs <- summary(melon.orthog.lm)$coef[,1]
cofs <- cbind(cofs)
mat.intercept %*% cofs


# EVEN BETTER WAY: using tapply
attach(melonData)
tapply(Yield, INDEX=list(Variety), mean)
# tapply gives the means directly!!!
detach(melonData)


# ------------------------------------------------------------------------------------------
# comparing the contrasts: 
contrasts(melonData$Variety) # for original model
melon.orthog.lm$contrasts # for the orthogonal model

# different summary coefs
cbind(summary(melon.orthog.lm)$coef[,1], summary(melon.lm)$coef[,1])

# now the anova SS are partitioned
anova(melon.orthog.lm)
anova(melon.lm)

# NOTICE: t-tests for coefs of ACvBD1 and AvC1 and BvC1 are same as F-tests
# and test the same hypotheses (because the contrasts are ORTHOGONALLY made)
summary(melon.orthog.lm)$coef
anova(melon.orthog.lm)
# From both summary and anova, we see the most or major effect is due to 
# rootstocks (contrast ACvBD, (AC come from one rootstock while BD come from 
# a different rootstock))
# Also: contrast BvD p = 0.007 means there is a difference in response
# between varieities B and D, with B making the largest mean yield of D. 





# Exercise -----------------------------------------------------------------

# H: mu_A = (muB + muC + muD)/3

AvBCD = C(melonData$Variety, contr=c(3,-1,-1,-1), 1)
AvD = C(melonData$Variety, contr=c(1,1,-1,-1), 1)
BvC = C(melonData$Variety, contr=c(-1,-1,1,1),1)

# fit the model
melon.diffAllThree.lm <- lm(Yield ~ AvBCD + AvD + BvC, data=melonData, x=TRUE)

# comparing contrasts
getContrastMatrix(melon.lm) # the original one
getContrastMatrix(melon.diffAllThree.lm) # the new one

# test that these are orthogonal (so that Variety SS's are split)
testContrastsOrthogonal(melon.diffAllThree.lm) # nope not orthogonal

# use the helmert method for orthogonal contrasts
helmerts = makeOrthogonalContrasts.df(factorNames=LETTERS[1:4], 
                        contrastNames=c("AvBCD","BvCD","CvD"), mirror=TRUE)
helmerts
# TODO this is not ready, FIX
contrs = makeOrthogonalContrasts.lm(data=melonData,
                                    contrastNames=c("AvBCD","BvCD","CvD"),
                                    mirror=TRUE); contrs


# fit the model
melon.diffAllThree.lm <- lm(Yield ~ AvBCD + BvCD + CvD, data=contrs, x=T)
melon.diffAllThree.lm
# or can use the formula maker
form = makeFormulaFromData(contrs); form
melon.diffAllThree.lm <- lm(form, data=contrs, x=T)
melon.diffAllThree.lm

anova(melon.diffAllThree.lm)
# conclude: the mean A is different from the mean yield of others B,C,D
# since p = 0.00046
# B is different in mean yield than C and D (p = 0.00000847)
# C is different in mean yield than D (p = 0.000437)



# ANOTHER WAY TO FIT: 
p = nlevels(melonData$Variety); p
melonData1 <- melonData
contrasts(melonData1$Variety) <- mirror(contr.helmert(p))
melon.diffAllThree.lm2 <- lm(Yield ~ Variety, data=melonData1)
# so now we see the t-tests are the same, and same to F-tests of the first
# model (diffAllThree) 
summary(melon.diffAllThree.lm2)
summary(melon.diffAllThree.lm)
#  but the F-test for the diffAllThree2 model does not have them split ...
anova(melon.diffAllThree.lm)
anova(melon.diffAllThree.lm2) # just refer to summary table (equiv to F-table)


# can also do this
summary(aov(Yield ~ Variety, data=melonData1),
        split=list(Variety=list(AvBCD=1, BvCD=2, CvD=3))) # same as Fs for diffallthree
anova(melon.diffAllThree.lm)





# Testing model assumptions

# 1) NORMALITY within populations
melonA = subset(melonData, Variety=="A")
melonB = subset(melonData, Variety=="B")
melonC = subset(melonData, Variety=="C")
melonD = subset(melonData, Variety=="D")
shapiro.test(melonA$Yield)
shapiro.test(melonB$Yield)
shapiro.test(melonC$Yield)
shapiro.test(melonD$Yield)
# no reason to detect the null of normal populations

# can also plot the entire data set (not for individual populations)
# BOXPLOTS
ggplot(data=melonData, aes(x=Variety, y=Yield, colour=Variety)) + 
      geom_boxplot(size=1)
# DENSITY PLOTS ... each only sort-of normal
ggplot(data=melonData, aes(x=Yield, colour=Variety)) + 
      geom_line(stat="density", size=2)
# overall density plot for all groups
ggplot(data=melonData, aes(x=Yield)) + 
      geom_line(stat="density", size=2)
# normal probability plot for all groups combined
library(ggfortify)
normalityPlot(melon.diffAllThree.lm, size=3, colour="blue")
plot(melon.diffAllThree.lm, which=2)
# normality plots for each group
qplot(sample=Yield, data=melonData, color=Variety, size=2)

#TODO: install.packages("qqplotr") for later R version and use the straight
# lines as comparisons. 
# using R docker which has qqplotr
#ggplot(data=melonData, mapping=aes(sample=Yield, color=Variety, fill=Variety)) + 
#  stat_qq_line() + stat_qq_point() + facet_wrap(~Variety) + 
#  stat_qq_band(alpha=0.5) + 
#  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# another resource here: (for geom qq line homemade)
#https://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2/


# 2) Equal Variances within populations : Levene's Test
library(car)
with(melonData, leveneTest(Yield, group=Variety))
# no reason to reject null of equal variances across varieties. 

# equal variance: Barlett's test
with(melonData, bartlett.test(Yield, g =Variety)) # fail rject null. 
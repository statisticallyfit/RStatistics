setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(show.signif.stars = FALSE)



# gre = GRE exam scores
# gpa = GPA scores
# rank = rank of undergraduate institution
# admit = response variable, whether or no the student (bservation) was admitted into
# his graduate school. 
greData <- read.table("data/binary.txt", header=TRUE)
head(greData)
is.factor(greData$admit)
greData$admit <- factor(ifelse(greData$admit == 1, "Yes", "No"))
greData$rank <- factor(greData$rank)

# First: check for empty cells (undesirable)
# Here not the case
xtabs(~ admit + rank, data=greData)


# Fit the model: 
admit.glm <- glm(admit ~ gre + gpa + rank, family=binomial, data=greData)

# Interpret the fit: 
anova(admit.glm, test="Chisq")

# INTERPRET: all predictors are significant. 
# First line: tests just the gre term model
# Second line: tests the gpa + gre model, with gre fitted first
# Third line: tests the full gre + gpa + rank model. Tests the rank term given
# that the gre + gpa terms were fitted. 

# Third line: the deviance = 21.826 with p-value = 0.00007088 so the rank term is
# significantly different from zero using the nestd test.
admit.gregpa.glm <- glm(admit ~ gre + gpa, family=binomial, data=greData)
anova(admit.gregpa.glm, admit.glm, test="Chisq")
# Or: 
NestedLikelihoodRatioTest(admit.gregpa.glm, admit.glm, printNice=T)
# So the rank term is indeed a significant predictor of admissions. 


# The reesidual deviance is significant, perhaps because there are other
# factors that impact admittance. 
ResidualDevianceTest(admit.glm, printNice = T)
# INTERPRET: the model is a not a good fit since residual deviance is large. 



### INTERPRET COEFFICIENTS AND CONFIDENCE INTERVALS:  ---------------------------------

# The profile likelihood confidence intervals: these do not assume normality
# and work better than the default CI's when sample sizes are small. 
CI.profile <- confint(admit.glm); CI.profile
# The default CI's
confint.default(admit.glm)


# NOTE: when odds ratio = 1, there is no difference in the probabilities. 

# Coefficient odds ratios and CI's odds ratios
exp(cbind(OR.coef = coef(admit.glm), OR.cis = CI.profile))
# -> GPA: for a one unit increase in GPA, the odds of being addmitted versus NOT
# being admitted increases by a factor of 2.23. 
# -> GPA Ci: does not contain 1, so the odds of being admitted increases significantly. 


# The percentage changes in response: 
100*(exp(cbind(OR.coef = coef(admit.glm), OR.cis = CI.profile))  - 1)

# ---> GRE: when GPA and rank are held constant, a 1 unit increase in GRE score increases
# the odds of being admitted by 0.2267%. 

# ---> GPA: when GRE, rank are held constant, then for a one unit increase in GPA, 
# the odds of being addmitted versus NOT being admitted increases by 123 %. 

# ---> RANK_2: when GRE and GPA are held constant, the odds of being admitted from a rank2
# university is 49.107% lower than the odds from a rank1 university. 
# (Or : when GRE and GPA are held constant, the odds of being admitted  for a rank2
# university over the odds of being admitted for a rank1 university is exp(-0.675) = 0.508. )

# ---> RANK_3: when GRE, GPA are constant, the odds of being admitted is 73.82% lower for
# a rank 3 university than from a rank1 university. 
# (Or: when GRE and GPA are held constant, the odds of being admitted for a rank3 over 
# the odds of being admitted  for a rank 1 university is exp(-1.34) = 0.261)

# ---> RANK_4: when GRE, and GPA are held constant, the odds of being admitted is
# 78.806% higher for a rank1 than a rank4 university. 




### RESETTING THE BASE LEVEL TO RANK 2: ------------------------------------------------

# Do the releveling to base level = rank2
greData2 <- greData
greData2$rank <- relevel(greData$rank, ref="2")
# compare: can see that relevelign was a success
levels(greData$rank)
levels(greData2$rank)

# Fit the model
admit2.glm <- glm(admit ~ gre + gpa + rank, data=greData2, family=binomial)


# INTERPRET THE COEFFICIENTS: 
cof <- cbind(summary(admit2.glm)$coef[,1])

cof # log odds 

exp(cof) # the odds 

100*(exp(cof) - 1) # percentage changes in odds 

# ---> RANK_1: holding gre, gpa fixed, the odds of being admitted to a rank 1 university
# is 96.490% higher than for a rank 2 university
# (Or: holding gre, gpa fixed, the odds of being admitted to a rank1 university over the
# odds of being admitted to a rank2 university is exp(0.675) = 1.964 = 1/0.509 from
# the previous model = 1 / exp(-0.675))

# ---> RANK_3: holding gre, gpa fixed, the odds of being admitted to a rank 3 university 
# is 48.560% lower than for a rank 2 university. 

# ---> RANK_4: holding gre, gpa fixed, the odds of being admitted to a rank 4 university 
# is 58.35% lower than for a rank 2 university. 



#### PREDICTIONS ----------------------------------------------------------------------

# Goal: to predict the probability of admission for students from each level of rank, 
# holding gre and GPA constant at the value of their means
newdf <- with(greData, data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4)))
newdf
newdf$rank.prob <- predict(admit.glm, newdata=newdf, type="response")
newdf

# verify manually: (for the rank1odel only (rank1 = 1, rank2=rank3=rank4 = 0))
cof 
# linear predictor for rank1 = 1 
n = -4.665422001295 + 0.002264425786*587.7 + 0.804037549280*3.3899 + 0.675442927964
exp(n) / (1 + exp(n)) # probability


# INTERPRET: the students from higher ranked universities have a higher probability of
# admission compared to students from lower-ranked universities, assuming GRE and GPA
# scores are held fixed. 

predict(admit.glm, type="response") # this predicts using the training data





#### 7.4 EBINOMIAL DATA -----------------------------------------------------------------

# Example 7.4.1: single quantitative predictor

# Data: 
# r = number of dead insects 
# n = corresponding sample of insects, after 5 hours exposure to dosages of CO2
# ldose = log10(dosage)
beetleData <- read.table("data/beetle.txt", header=TRUE)
head(beetleData)

 #Plot shows a strong positive linear association between log(dose) and proportion
# so it seems logistic fits well?
# TODO: or should this be sigmoid for the data to recommend logistic model?
ggplot(data=beetleData, aes(x=ldose, y=r/n)) + geom_point(size=3, color="dodgerblue") +
      geom_line() + ggtitle("Log Dose versus Proportion of Dead Beetles")


# Fit the single predictor model
beetle.glm <- glm(r/n ~ ldose, family=binomial, weights = n, data=beetleData)
# note: need to take the differing sample sizes into account by saying: weights = n

anova(beetle.glm, test="Chisq")

# INTERPRET: 
# Deviance = 272.97 with df = 1, so p-value < 2.2e-16 zero, so the ldose predictor
# is a significant predictor of the proportion of dead beetles. 

# Residual deviance =  11.23 with df = 6. P-value is not TOO small: = 0.0815...
1 - pchisq(11.23, df=6)
# ... so  expected successes and observed successes are similar ==> model is a good fit.

# REMEMBER df formulas: 
# df.residual = n - k - 1 
# df.deviance = (n-1) - df.residual = k = num predictors

# Fitting another way to avoid the weights argument: (then can use my functions)
n <- beetleData$n
r <- beetleData$r 
deadBeetleCount <- cbind(r, n- r)

beetle.count.glm <- glm(deadBeetleCount ~ ldose, family=binomial, data=beetleData)

# The tests are the same: 
anova(beetle.count.glm, test='Chisq')

# The models are the same: 
summary(beetle.glm)
summary(beetle.count.glm)

DevianceTest(beetle.count.glm)
ResidualDevianceTest(beetle.count.glm)



# INTERPRET COEFFICIENTS: -------------------------------------------------------------
cof <- cbind(summary(beetle.glm)$coef[,1]); cof 

exp(cof)

100*(exp(cof)-1)

# ---> LDOSE: for a 1 unit increase in log(dose), the odds of beetles dying versus not
# dying increases by 7.645% . 
# (Or: the odds of beetles dying over odds of beetles not dying is exp(34.27) = 7.64e14)



# Plot the confidence bands with the fitted glm on the data: 
plotConfidenceBands.glm(beetle.glm)


### MEDIAN LETHAL DOSE: LD50 ------------------------------------------------------------

# median lethal dose (LD(50)) = dose required to kill half the animals. 
# = the value of X for which p = 1/2: 
# ln(pi/(1-pi)) = B0 + B1 * Xi

# CALCULATE ld50: -----
# ln(1/2 / 1/2) = ln(1) = 0 = B0 + B1 * Xi 
# ==>   Xi = -B0 / B1      is the median lethal dose

# For our model: 
ld50 = - cof[1] / cof[2]; ld50  # this is log(dose). Need to exponentiate to get dose.
10^(ld50) # exponentiate base 10 since predictor is log10(dose)
# So dosage in mg / liter required to kill half th ebeetles is 59.118 mg / liter


# Calculate LD25: -----
# ln(1/4 / (3/4)) = B0 + B1*X_i  ===> ln(1/3) = -ln(3) = B0 + B1X_i
# ==>   X_i = (-ln(3) - B0)/B1
ld25 = -(log(3) + cof[1]) / cof[2]; ld25
10^ld25 # exponentiate base 10 since predictor is log10(dose)
 #So the dosage required to kill 25% of the animals is 54.91 mg / liter. 
# Or: 25% of insects will be killed with a carbon disulphid dose of 55 mg / liter.


# Calculate LD75: -----
# ln(3/4 / (1/4)) = ln(3) = B0 + B1X_i ===> X_i = (ln(3) - B0)/B1
ld75 = (log(3) - cof[1])/cof[2]; ld75
10^ld75  # exponentiate base 10 since predictor is log10(dose)
# Means: 75% of insects will be killed with a dose of 63.647 mg / liter. 


# PREDICTED VALUES: -------------------------------------------------------------------
devianceResiduals <- residuals(beetle.glm, type="deviance")
# or can get them this way: 
s <- summary(beetle.glm)
s$deviance.resid

preds <- predict.glm(beetle.glm, se.fit=TRUE, type="response")
class(preds)
names(preds)

# convert probability to predicted counts counts
fits <- preds$fit * beetleData$n

beetlePredictions <- cbind(beetleData[,cbind(2,3)], fits, devianceResiduals)
beetlePredictions

# Can check if the model is good by checking normality of deviance residuals
residualFitPlot(beetle.glm)
autoplot(beetle.glm) 
# residuals : non normal! deviation from straight line. Overestimates probabilities
# near 0.5 and underestimates probabilities at extreme fitted values. 

shapiro.test(residuals(beetle.glm)) # not significant, fail to reject H0, no evidence
# of non normality (but this is only due to the small sample size)

# CONCLUDE: cannot read too much into these plots because of the small sample size. 


### Example 7.4.4 --------------------------------------------------------------------
 
# Completely randomized design where errors are binomial. 
# Examine the gender ratios in different genotypes (A, B, C< D) of an insect. 
# Question: is the proportion of male to female offspring produced the same for different
# genotypes? 
ratioData <- read.table("data/sexratio.txt", header=TRUE)
head(ratioData)

# Assume binomial distribution for the proportion of males: Y = male/total
ratio.glm <- glm(male/total ~ genotype, data=ratioData, family=binomial, weights=total)
anova(ratio.glm, test="Chisq")

# INTERPRET ANOVA: 

# Deviance = 17.736 with df = 3, so p-value = 0.00049 ===> means the model is statistically
# useful (different from the null model)

# Residual deviance is 14.51, df = 10, with p-value = 0.15 > 0.05 so fail to reject H0
# that expected successes are same as observed successes. Thus the model is a good fit.
1 -pchisq(14.51, df=10)


# Fitting the model using the "count" method
count <- as.matrix(cbind(male=ratioData[,1], female=ratioData[,2]-ratioData[,1]))
ratio.count.glm <- glm(count ~ genotype, data=ratioData, family=binomial)

# check the models are the same:
summary(ratio.count.glm)
summary(ratio.glm) 

DevianceTest(ratio.count.glm) # so the effect of genotype is highly significant. 
# In other words: genotype is a statistically significant predictor of male/total. 
ResidualDevianceTest(ratio.count.glm)



# Plotting: boxplots of gender ratios for different genotypes:
ggplot(data=ratioData, aes(x=genotype, y=male/total, colour=genotype)) +
      geom_boxplot(size=1) + ggtitle("Offspring gender proportion vs Insect Genotype")
# Significant difference in proportion whenever the boxplots are not overlapping: 
# A, C
# C, D and for C, B

# INTERPRET COEFFICIENTS: 
cof <- summary(ratio.glm)$coef[,1]
cof 
pvalues <- summary(ratio.glm)$coef[,4]

cbind(Odds=exp(cof), OddsPercentChange=100*(exp(cof) - 1),Pvalues=pvalues)

# ---> genotypeB: the odds of male vs odds of female is 38.18% lower for a genotypeB
# versus a genotypeA insect. This is not significant (p = 0.40)

# ---> genotypeC: the odds of a male offspring vs odds of female offspring is 184% higher
# for a genotypeC versus genotypeA insect, and this difference is significant. (p = 0.0126)

# ---> genotypeD: the odds of a male vs female offspring is 51.48% lower for genotypeD
# than for genotypeC insect, and this difference is not significant (p = 0.1377)


# RESETTING BASE LEVEL TO genotypeC -----------------------------------------------------

ratioDataC <- ratioData
ratioDataC$genotype <- relevel(ratioData$genotype, ref="C")
levels(ratioDataC$genotype) # has base level C (C is first)
levels(ratioData$genotype) # has base level A (A is first)

ratioC.glm <- glm(male/total ~ genotype, weights=total, family=binomial, data=ratioDataC)

anova(ratioC.glm, test="Chisq") # same interpretation as when baselevel = "A"

# INTERPRET COEFFS: 

cofC <- summary(ratioC.glm)$coef[,1]
pvalues <- summary(ratioC.glm)$coef[,4]

cbind(Odds=exp(cofC), OddsPercentChange=100*(exp(cofC)-1), PValues=pvalues)

# ---> genotypeA: the odds of having a male offspring is 64.83% lower for genotype A than 
# for genotypeC (and this difference is significant: p = 0.0126)
# ---> genotypeB: the odds of having a male offspring is 78.26% lower for genotype B than 
# for genotypeC (and this difference is significant: p = 0.007)
# ---> genotypeD: the odds of having a male offspring is 82.93% lower for genotype A than 
# for genotypeC (and this difference is significant: p = 0.00019)


### *** NOTE: Multiple comparisons are tests of factors by releveling (as above). 
# But dangerous because risk of type 1 error rises as you do more hypothesis tests. 
# The more null hypotheses there are to be tested, the more likely one of them is to be 
# rejected even if they are true. The probability of incorrectly rejecting at least one
# H0 (null) is much larger than the alpha significance level and continues to increase
# with each additional test. 

# Better method: ask the questions before the expierment, then create contrasts to 
# partition the deviance for genotype. 


#### ===================================================================================
# SOURCE: for interpreting coefficients in logistic regression: 
# https://hyp.is/k2ngErQqEem0qnt7FOUWlw/stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
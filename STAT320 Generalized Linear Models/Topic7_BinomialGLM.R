setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(show.signif.stars = FALSE)

greData <- read.table("data/binary.txt", header=TRUE)
head(greData)

# gre = GRE exam scores
# gpa = GPA scores
# rank = rank of undergraduate institution
# admit = response variable, whether or no the student (bservation) was admitted into
# his graduate school. 
greData$rank <- factor(greData$rank)

# First: check for empty cells (undesirable)
# Here not the case
xtabs(~ admit + rank, data=greData)


# Fit the model: 
admit.glm <- glm(admit ~ gre + gpa + rank, family=binomial, data=greData)
anova(admit.glm, test="Chisq")
# INTERPRET: all predictors are significant. 
# First line: tests just the gre term model
# Second line: tests the gpa + gre model, with gre fitted first
# Third line: tests the full gre + gpa + rank model. Tests the rank term given
# that the gre + gpa terms were fitted. 

# The reesidual deviance is significant, probability because there are other
# factors that impact admittance. 
ResidualDevianceTest(admit.glm, printNice = T)
# INTERPRET: the model is a good fit. 

summary(admit.glm)


### CONFIDENCE INTERVALS: 

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
# -> GPA: for a one unit increase in GPA, the odds of being addmitted versus NOT
# being admitted increases by 123 %. 
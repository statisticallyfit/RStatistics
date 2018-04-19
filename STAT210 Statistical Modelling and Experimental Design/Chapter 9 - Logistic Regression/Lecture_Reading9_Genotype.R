setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 9 - Logistic Regression/")

library(ggplot2)

options(digits=10, show.signif.stars = FALSE)

# gender ratios in different genotypes (A, B, C, D) of insect) 
# Ask:  is the proportion of male to female offspring produced the same for different
# genotypes? 
genderData <- read.table("gender.txt", header=TRUE)
is.factor(genderData$genotype)

attach(genderData)
plot(male/total ~ genotype)

ggplot(genderData, aes(x=genotype, y=male/total, group = genotype, color=genotype)) + 
      geom_boxplot(size=1)
# INTERPRET: mean seems different for A-C but not A-B, and A-D
# Mean seems different for B-C but not B-D, 
# Mean seems different for C-D. 
# Much variance for D compared to others, and little variance for C. 


# MODEL (no need for weights = total when using counts)
count <- cbind(genderData$male, genderData$total - genderData$male)
ratio.glm <- glm(count ~ genotype, family=binomial, data=genderData)
ratio.null.glm <- glm(count ~ 1, family=binomial, data=genderData)
summary(ratio.glm)


# RESIUDAL DEVIANCE (individual model comparison, global fit)
# test method 1
anova(ratio.glm, test="Chisq") # the p-value is for the deviance
# test method 2
ResidualDevianceTest(ratio.glm)


# DEVIANCE (nested test with null model)
anova(ratio.null.glm, ratio.glm, test="Chisq")
DevianceTest(ratio.glm) # the effect of genotype is highly significant


# Coefficient z-tests of chisquare with df = 1
cof <- summary(ratio.glm)$coef
# (beta/sd)^2 has chi square dist with df = 1
1 - pchisq((cof[,1] / cof[,2])^2, df=1)

# We can see that A is highly different from zero (p = 0.000135)
# mean diff between A-B and A-D is not significant (p =0.4, p = 0.14)
# mean diff between A-C is significant (p=0.0126)

# From boxplot, we see C is different from B and D (but need to test)




# FITTING MODEL WITH BASE = C to test difference between B and D
detach(genderData)

genderData.C <- genderData
genderData.C$genotype <- relevel(genderData$genotype, ref="C")
# check that releveling was done successfully:
levels(genderData.C$genotype) # starts with C
levels(genderData$genotype) # starts with A

count <- cbind(genderData$male, genderData$total - genderData$male)
ratio.C.glm <- glm(count ~ genotype, family=binomial, data=genderData.C)

summary(ratio.C.glm)
# INTERPRET: 
# signif difference in mean between C-B (p=0.007) and C-D (p=0.00019)

anova(ratio.C.glm)

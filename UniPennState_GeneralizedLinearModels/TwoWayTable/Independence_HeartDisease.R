source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

#install.packages("DescTools")
#install.packages("agricolae")
#library(DescTools) ## problem - doesn't return actual values!! helP?
#library(agricolae)

# CHD = coronary heart disease
heart <- matrix(c(12,8,31,41, 307,246,439,245), ncol=4, byrow = TRUE,
                dimnames = list(CHD=c("chd", "no-chd"),
                                SerumCholesterolLevels=c("0-199","200-219","220-259","260+")))
heart 

## Chi-square test of independence
# TODO why does it matter if we correct Yates or not for 2x2 tables
# but not for IxJ tables?
heart.chi <- chisq.test(heart); heart.chi
heart.chi$observed
heart.chi$expected
heart.chi$residuals  # pearson residuals
# adjusted pearson residuals (distr N(0,1) so too big if squared stdres are bigger than 3.84)
heart.chi$stdres
(heart.chi$stdres)^2 
(heart.chi$residuals)^2


## Likelihood Ratio Test
LikelihoodRatioTest(heart)

## Deviance Residuals
devianceResiduals(heart)


## Conditional probabilities
# margin=2 means marginal for cols
serumMarginals <- margin.table(heart, margin=2); serumMarginals
heartMarginals <- margin.table(heart, margin=1); heartMarginals


# counts of four groups with CHD
heart[1,]
# counts of four groups with NO CHD
heart[2,]

# conditional probabilities are:
heart[1,] / serumMarginals
heart[2,] / serumMarginals

# easier way: 
prop.table(heart, margin=1)
prop.table(heart, margin=2)
rowProbabilityHat(heart) # P(X=i | Y=j), where X = serum levels, Y = CHD or not. 
colProbabilityHat(heart) # P(Y=j | X=i), where X = serum levels, Y = CHD or not. 

########## USing VCD
library(vcd)

assocstats(heart)

Desc(heart, verbose = "high")

########## Using custom functions. 
relativeRisk(heart)

oddsRatio(heart)
OddsRatioCI(heart)
FisherExactTest(heart)





########################## Measures of Association in IxJ tables ###########################

# can do difference of proportions test between 0.038 and 0.031 in: 
colProbabilityHat(heart)
TwoPropPooledTestTable(heart, 1,1, 1,2)
# Or do chi-square for that section of the table
chisq.test(heart[1:2, 1:2], correct = FALSE)

source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

## When total sample size AND expected cell counts are small, we use exact tests. 
## When X and Y are fixed (not random), the p-value comes from Hypergeometric distribution.

tea <- matrix(c(3,1,1,3), ncol=2, dimnames=list(Poured=c("Tea", "Milk"),
                                                LadySays=c("Tea", "Milk")))
tea #

### Test for independence (EXACT)
FisherExactTest(tea, alternative = "two.sided")
FisherExactTest(tea, alternative = "greater")
FisherExactTest(tea, alternative = "less")

# Chi-square
chi.tea <- chisq.test(tea); chi.tea #
chi.tea$residuals
chi.tea$stdres
chi.tea$expected

chi.tea.noyates <- chisq.test(tea, correct = FALSE); chi.tea.noyates
chi.tea.noyates$residuals
chi.tea.noyates$stdres
chi.tea$expected


# Likelihood Ratio Test
LikelihoodRatioTest(tea)


## Manually? 
# phyper how? 


# Percentages
rowProbabilityHat(tea)
colProbabilityHat(tea)

# Props
propDifferences(tea)
TwoPropPooledTestTable(tea)
TwoPropCITable(tea)

# Relrisk
relativeRisk(tea)
RelativeRiskCI(tea)

# Odds ratio
oddsRatio(tea)
OddsRatioCI(tea) # if this gives error, q() because it is using epitools oddsratio
FisherExactTest(tea)


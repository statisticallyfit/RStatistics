source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

#install.packages("epitools")
library(epitools)
detach("package:epitools", unload=TRUE)

smoke <- matrix(c(400,416,188,1380,1823,1168), ncol=2,
                dimnames=list(ParentSmoke=c("both","one","neither"),
                              StudentSmoke=c("Yes", "No")))
smoke

### Chi-square independence test
chi.smoke <- chisq.test(smoke); chi.smoke
rowProbabilityHat(smoke)
colProbabilityHat(smoke)
chi.smoke$expected

### Likelihood ratio test
LikelihoodRatioTest(smoke)

### Gamma Goldman and kruskal's agmma
GoodmanKruskalGamma(smoke)

#### All significant so the parent smoke and student smoke are related. 


################## Manual
library(vcd)

assocstats(smoke)
# compare expected with plot
chi.smoke$expected
chi.smoke$residuals
chi.smoke$stdres
mosaic(smoke)
mosaic(smoke, split_vertical = TRUE)

assoc(smoke)



##################
marginalTable(smoke)

# relative risks
relativeRisk(smoke)
RelativeRiskCI(smoke)

# prop diffs
propDifferences(smoke)
TwoPropPooledTestTable(smoke)
TwoPropCITable(smoke)

# odds ratios
oddsRatio(smoke)
OddsRatioCI(smoke)
FisherExactTest(smoke)

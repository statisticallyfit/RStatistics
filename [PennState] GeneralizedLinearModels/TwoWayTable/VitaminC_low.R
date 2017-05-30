source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

ski<-matrix(c(3.1, 1.7, 10.9, 12.2), ncol=2, 
            dimnames=list(Treatment=c("Placebo", "VitaminC"), 
                          Cold=c("Cold", "NoCold")))
ski

#### Let us look at the Percentage, Row Percentage and Column Percentages 
#### of the total observations contained in each cell.
## SAME FOR HIGH, LOW, AND MEDIUM data files
percentage <- 100*ski/sum(ski)
rowSums <- rowSums(ski)
rowPercentage <- 100*rbind(ski[1,]/rowSums[1],ski[2,]/rowSums[2])
colSums <- colSums(ski)
colPercentage <- 100*cbind(ski[,1]/colSums[1],ski[,2]/colSums[2])
percentage
rowPercentage
colPercentage

#### Pearson's Chi-squared test with Yates' continuity correction
result <- chisq.test(ski); result
result$observed
result$expected
result$residuals

#### Pearson's Chi-squared test  WITHOUT Yates' continuity correction
result <- chisq.test(ski, correct=FALSE); result
result$observed
result$expected
result$residuals

#### Likelihood Ratio Chi-Squared Statistic
likelihoodRatioTest(ski)

#### Fisher's Exact Test
fisher_Exact_TwoSided=fisher.test(ski,alternative = "two.sided")
fisher_Exact_Less=fisher.test(ski,alternative = "less")
fisher_Exact_Greater=fisher.test(ski,alternative = "greater")
rbind(fisher_Exact_TwoSided, fisher_Exact_Less, fisher_Exact_Greater)

source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

ski <- matrix(c(310, 170, 1090, 1220), ncol=2, 
              dimnames=list(Treatment=c("Placebo", "VitaminC"), 
                            Cold=c("Cold", "NoCold")))
ski 

# Percentage of Row and Col and of Total observations in each cell
## SAME FOR HIGH, LOW, AND MEDIUM data files
percentage <- 100 * ski / sum(ski)
rowSums <- rowSums(ski); rowSums
rowPercentage <- 100 * rbind(ski[1,] / rowSums[1], ski[2,] / rowSums[2])
colSums <- colSums(ski); colSums 
colPercentage <- 100 * cbind(ski[,1]/colSums[1], ski[,2]/colSums[2])
percentage
rowPercentage
colPercentage


# Chi-Squared test of Independence with Yates continuity correction
result <- chisq.test(ski); result
result$observed
result$expected
result$residuals

# Chi-Squared test of Independence WITHOUT Yates continuity correction
result <- chisq.test(ski, correct=FALSE); result
result$observed
result$expected
result$residuals

# Likelihood-Ratio Test
likelihoodRatioTest(ski)


# Column 1 Risk Estimates
rowSums 
colSums
# placebo_cold / placebo  = 31 / 140 = 0.2214
risk1.col1 <- ski[1,1] / rowSums[1]; risk1.col1 <- unname(risk1.col1)
risk1.col1
# vitC_cold / vitC  =  17 / 139 = 0.1223
risk2.col1 <- ski[2,1] / rowSums[2]; risk2.col1 <- unname(risk2.col1)
risk2.col1
rho1 <- risk1.col1 / risk2.col1; rho1 <- unname(rho1) 
rho1 
# placebo and vitC _ COLD / total 
total1 <- colSums[1] / sum(rowSums); total1 <- unname(total1)
total1
# 17/139 - 31/140
diff1 <- risk2.col1 - risk1.col1; diff1 # difference of proportions

cold <- rbind(risk1.col1, risk2.col1, total1, diff1)
colnames(cold) <- "Cold"
cold 


# Confidence interval of difference in proportions of column 1 (cold)
SE_diff1 <- sqrt(risk1.col1 * (1 - risk1.col1) / unname(rowSums[1]) + 
                       risk2.col1 * (1 - risk2.col1) / unname(rowSums[2]))
SE_diff1
z.crit <- qnorm(0.975); z.crit 
CI_diff1 <- cbind(diff1 - z.crit * SE_diff1, diff1 + z.crit*SE_diff1); CI_diff1


# Column 2 risk estimates (No Cold)
# placebo_nocold / placebo = 109/140
risk1.col2 <- ski[1,2] / rowSums[1]; risk1.col2 <- unname(risk1.col2)
risk1.col2
# vitC_nocold / vitC = 122/139
risk2.col2 <- ski[2,2] / rowSums[2]; risk2.col2 <- unname(risk2.col2)
risk2.col2
# nocold / total = 231 / 279
total2 <- colSums[2] / sum(colSums); total2 <- unname(total2) 
total2 
#122/139 - 109/140
diff2 <- risk2.col2 - risk1.col2; diff2

noCold <- rbind(risk1.col2, risk2.col2, total2, diff2)
colnames(noCold) <- "NoCold"
noCold


# Confidence interval for difference of proportions for col 2 (no cold)
SE_diff2 <- sqrt(risk1.col2*(1-risk1.col2)/unname(rowSums[1]) + 
                       risk2.col2*(1-risk2.col2)/unname(rowSums[2]))
SE_diff2
CI_diff2 <- cbind(diff2 - z.crit*SE_diff2, diff2 + z.crit*SE_diff2); CI_diff2


# Estimate of odds of the two rows 
odds1 <- risk2.col1 / risk1.col1; odds1 # 17/139 / 31/140
odds2 <- risk2.col2 / risk1.col2; odds2 # 122/139 / 109/140
      
# Odds Ratio - cold to no cold
oddsRatio.cold.none <- odds1 / odds2; oddsRatio.cold.none

# Confidence Interval of odds ratio
log_CI <- cbind(log(oddsRatio.cold.none) - z.crit*sqrt(sum(1/ski)), 
                log(oddsRatio.cold.none) + z.crit*sqrt(sum(1/ski)))
log_CI
oddsRatio.cold.none_CI <- exp(log_CI); oddsRatio.cold.none_CI


##################################################################################

# Using the vcd package
library(vcd)

# to get deviance, pearson chi, and others
assocstats(ski)
likelihoodRatioTest(ski)
chisq.test(ski, correct = FALSE)

# odds ratio, confint
oddsratio(ski, log=FALSE)
lor <- oddsratio(ski); lor
confint(lor)      # CI on log scale 
exp(confint(lor)) # CI on basic scale

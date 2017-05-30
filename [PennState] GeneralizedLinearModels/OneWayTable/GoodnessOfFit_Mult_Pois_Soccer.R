source("/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R")
source("/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/OneWayTable/PoissonSampling_Soccer.R")

## (I) Goodness of fit Multinomial null model (ignoring LAMBDA estimate) --------------

soccer
multinom.test <- chisq.test(soccer$Freq); multinom.test
multinom.test$statistic
multinom.test$p.value
multinom.test$residuals

# output table
df.mult <- length(soccer$Freq) - 1
output.mult <- data.frame(goals=0:8, O_j=multinom.test$obs, 
                     E_j=multinom.test$exp, 
                     res_j=multinom.test$res,
                     cutoff=sqrt((df.mult-1)/df.mult))
output.mult

# likelihood ratio test --- zero problem
likelihoodRatioTest(multinom.test$observed)
# deviance residuals --- zero problem
devianceResiduals(multinom.test$obs, multinom.test$exp)


# likelihood ratio test --- adding 1/2 count to cells
multinom.test  # without 1/2 to cells
multinom.test.half <- chisq.test(soccer$Freq + 0.5); multinom.test.half
likelihoodRatioTest(multinom.test.half$observed, multinom.test.half$expected)
# deviance residuals ---- adding 1/2 count to cells
devianceResiduals(multinom.test.half$obs, multinom.test.half$exp)

# another (less common) solutions is to do the calculations with all values 
# greater than zero for G2 but that fixes the zero cell counts to so we are 
# estimating two less paramaters in this example thus the degrees of freedom 
# should be adjusted 
# option na.rm=TRUE removes all zero values from the calculation
G2 <- 2 * sum(multinom.test$obs * log(multinom.test$obs / multinom.test$exp), 
              na.rm=TRUE); G2
1-pchisq(G2, 6) # how to calculate df? 

# CONCLUSION: Multinomial model doesn't fit well even before nor after adding 1/2
# count to each cell - multinomial is not a good fit. 




### (II) Goodness of fit test Poisson null model (with lambda est.) ----------------
pi.hat 
# warning because some expected cell counts are less than 0.2
pois.test <- chisq.test(soccer$Freq, p = pi.hat, rescale.p = TRUE); pois.test 
pois.test$expected
pois.test$observed
pois.test$statistic
pois.test$p.value
pois.test$residuals


# showing that we are using poisson null model now, and before, the multinomial
multinom.test$expected
pois.test$expected


# dfs are not correct because we estimated lambda first then pi.hats (explain more help)
df.pois.good <- length(soccer$Freq) - 1 - 1; df.pois.good
chi.stat <- data.frame(pois.test$statistic)[,1]; chi.stat 
1 - pchisq(chi.stat, df.pois.good) # p.value

# output table
# notice that the model fits well without the cell with large residual. 
output.pois <- data.frame(goals=0:8, obs=pois.test$obs, exp=pois.test$exp, 
                          res=pois.test$residuals, 
                          cutoff=sqrt((df.pois.good-1)/df.pois.good))
output.pois
output.mult

# new - do calculations without zeros
G2 <- 2 * sum(pois.test$obs * log(pois.test$obs / pois.test$exp), na.rm=TRUE)
G2
1-pchisq(G2, df.pois.good)

# CONCLUSION: the model is not rejected, so poisson fits well (passed test
# AND residuals are not too large)
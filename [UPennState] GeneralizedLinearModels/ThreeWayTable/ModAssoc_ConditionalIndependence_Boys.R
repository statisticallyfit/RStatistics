source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_MutualIndependence_Boys.R')
source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_MarginalIndependence_Boys.R')
source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable/ModAssoc_JointIndependence_Boys.R')


##############################    TEST CONDITIONAL INDEPENDENCE    ############################
###############################################################################################

# NOTE: only for conditional tests do we need to add the chi.stats from
# every single test on each conditional table to get the overall chi.stat

# easy way
ConditionalIndependence(temp.by.SES)
ConditionalIndependence(temp.by.scout)
ConditionalIndependence(temp.by.delinq)
ConditionalIndependence(temp.scout.delinq)

CochranMantelHaenszelTest(temp.by.SES)

# WARNING: (assumption) are the conditional odds ratios comparable in size? 
oddsRatio(temp.by.SES[,,1])
oddsRatio(temp.by.SES[,,2])
oddsRatio(temp.by.SES[,,3])


# mantel-haenszel for three way
mantelhaen.test(temp.by.SES, correct = FALSE) # dependent (2x2xk test)
mantelhaen.test(temp.by.delinq, correct = FALSE)  # independent (generalized test)
mantelhaen.test(temp.scout.delinq, correct = FALSE) # independent (generalized test)



# longer way
############### CONDITIONAL on different levels of SES
# CONDITIONAL ON SES = low
temp.by.SES[,,1]
t1 <- ChiSquareIndependence(temp.by.SES[,,1]) # conditionally dependent,  scout delinquent
OddsRatioCI(temp.by.SES[,,1])

# CONDITIONAL ON SES = med
temp.by.SES[,,2]
t2 <- ChiSquareIndependence(temp.by.SES[,,2]) # conditionally dependent,  scout delinquent
OddsRatioCI(temp.by.SES[,,2])

# CONDITIONAL ON SES = high
temp.by.SES[,,3]
t3 <- ChiSquareIndependence(temp.by.SES[,,3]) # conditionally dependent,  scout delinquent
OddsRatioCI(temp.by.SES[,,3])

# To test the conditional model (BS, DS), we add up the individual chi.stats:
chi.stat <- t1$ChiStatistic + t2$ChiStatistic + t3$ChiStatistic; chi.stat
df <- t1$DegFree + t2$DegFree + t3$DegFree; df
1 - pchisq(chi.stat, df) # so cond. ind. model fits very well (don't reject)


############### Conditional on different levels of delinquent
# CONDITIONAL ON Delinquent = NO
temp.by.delinq[,,1]
ChiSquareIndependence(temp.by.delinq[,,1])
OddsRatioCI(temp.by.delinq[,,1])

# CONDITIONAL ON Delinquent = YES
temp.by.delinq[,,2]
ChiSquareIndependence(temp.by.delinq[,,2])
OddsRatioCI(temp.by.delinq[,,2])


############### Conditional on different levels of scout
# CONDITIONAL ON Scout = YES
temp.by.scout[,,1]
ChiSquareIndependence(temp.by.scout[,,1])
OddsRatioCI(temp.by.scout[,,1])

# CONDITIONAL ON scout = NO
temp.by.scout[,,2]
ChiSquareIndependence(temp.by.scout[,,2])
OddsRatioCI(temp.by.scout[,,2])



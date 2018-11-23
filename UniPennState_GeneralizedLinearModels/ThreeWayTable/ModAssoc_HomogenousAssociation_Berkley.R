source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')
setwd("/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/ThreeWayTable")


### Building data: --------------------------------------------------------------------------
berkeley <- read.table("berkeley.txt")
colnames(berkeley) <- c("D", "S", "A", "count")
temp <- xtabs(berkeley$count ~., data=berkeley)
ftable(temp)
Count <- berkeley$count; S <- berkeley$S; A <- berkeley$A; D <- berkeley$D 
temp.by.dept <- xtabs(Count ~ S + A + D); temp.by.dept
ftable(temp.by.dept)


# table of S*A without conditioning on Department (all three are the same!)
addmargins(temp.by.dept)[1:2, 1:2, 7] # the marginal table (SA, m = D)
margin.table(temp.by.dept, c(2,3))
tableSA_marg_D <- xtabs(berkeley$count ~ berkeley$S + berkeley$A); tableSA_marg_D

## The Steps (fisher, ... etc)
ConditionalIndependence(temp.by.dept)

fishers <- function(tbl){
      FisherExactTest(tbl)
      FisherExactTest(tbl, alt="greater")
      FisherExactTest(tbl, alt="less")
}
# Fisher tests for all the departments
fishers(temp.by.dept[,,1])
fishers(temp.by.dept[,,2])
fishers(temp.by.dept[,,3])
fishers(temp.by.dept[,,4])
fishers(temp.by.dept[,,5])
fishers(temp.by.dept[,,6])


# Relative risks and odds ratio groups
relsOdds <- function(tbl){
      relativeRisk(tbl)
      OddsRatioCI(tbl)
}
relsOdds(temp.by.dept[,,1])
relsOdds(temp.by.dept[,,2])
relsOdds(temp.by.dept[,,3])
relsOdds(temp.by.dept[,,4])
relsOdds(temp.by.dept[,,5])
relsOdds(temp.by.dept[,,6])


summary(glm(berkeley$count ~ D + S + A + D*S, family=poisson(link="log")))


### Prepared data:---------------------------------------------------------------------------
UCBAdmissions
dim(UCBAdmissions)
ftable(UCBAdmissions)

# marginal tests
MarginalIndependence(UCBAdmissions, correct = TRUE)

#conditional tests
ConditionalIndependence(UCBAdmissions, correct = TRUE)
CochranMantelHaenszelTest(UCBAdmissions, correct = TRUE) # that cond. odds ratios are all = 1

# mutual
MutualIndependence(UCBAdmissions)

# joint
JointIndependence(UCBAdmissions)

# homogenous
HomogenousAssociation(UCBAdmissions)

# Cochran-Mantel
# check assumptions that odds ratios are similar in magnitude and in same direction
oddsRatio(temp.by.dept[,,1])
oddsRatio(temp.by.dept[,,2])
oddsRatio(temp.by.dept[,,3])
oddsRatio(temp.by.dept[,,4])
oddsRatio(temp.by.dept[,,5])
oddsRatio(temp.by.dept[,,6])

CochranMantelHaenszelTest(UCBAdmissions, correct = TRUE)

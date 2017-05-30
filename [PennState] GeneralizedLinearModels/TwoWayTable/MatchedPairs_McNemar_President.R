source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

########################## Longitudinal design ##############################
vote.long <- matrix(c(794,150,86,570), ncol=2, byrow=TRUE,
                 dimnames=list(Time1=c("approve","disapprove"),
                               Time2=c("approve","disapprove")))
vote.long


##### McNemar Test without correction and with
McNemarTest(vote.long, correct = FALSE)
McNemarTest(vote.long)
# CONCLUSION: strong evidence that votes did change from Time1 to Time2. 
# Reject null of marginal proportions being the same (of dependence)



########################## Cross-sectional design ##############################
vote.cross <- matrix(c(944,880,656,720),nrow=2,dimnames=list(
      Survey=c("1st Survey", "2nd Survey"),
      Approval=c("approve", "disapprove")))
vote.cross # the marginal sums. 
vote.long

ChiSquareIndependence(vote.cross)
McNemarTest(vote.cross) #applies when marginal homogeneity is of interest
ChiSquareIndependence(vote.long)

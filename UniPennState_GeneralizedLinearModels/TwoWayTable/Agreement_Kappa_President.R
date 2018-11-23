source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')


#install.packages("vcdExtra")
library(vcdExtra)

########################## Longitudinal design ##############################
vote.long <- matrix(c(794,150,86,570), ncol=2, byrow=TRUE,
                    dimnames=list(Time1=c("approve","disapprove"),
                                  Time2=c("approve","disapprove")))
vote.long


### Simple kappa coefficient
prop <- vote.long/sum(vote.long); prop #
Po <- sum(diag(prop)); Po 
Pe <- rowSums(prop)[1] * colSums(prop)[1] + rowSums(prop)[2] * colSums(prop)[2]; Pe #
kappa <- (Po - Pe)/ (1 - Pe); kappa 
# how to find variance manually? 
#kappa.var <- 1/n * (Po*(1-Po)/(1-Pe)^2 + 
#                          2*(1-Po)*(2*Po*Pe - Po*(rowSums(prop) + colSums(prop)))/(1-Pe)^3 + 
#                          (1-Po)^2*(n*(colSums(prop)[2] + rowSums(prop)[1])^2 -4*Pe^2)/(1-Pe)^4)

### Automatically 
CohenKappaTest(vote.long)

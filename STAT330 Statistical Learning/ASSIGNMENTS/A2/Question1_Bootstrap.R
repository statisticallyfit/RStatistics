setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A2/")

library(ISLR)
library(MASS)

library(ggplot2)



# part g) plot that displays the probability of the jth observation is the bootstrap----------

# observation (or equivalaently, that the jth obs is in the bootstrap sample)
probability <- function(n) return(1 - (1 - 1/n)^n)
n <- 1:100000
limit <- 1 - exp(-1)
p <- probability(n)
df <- data.frame(n = n, p = p)

ggplot(data=df, aes(x=n, y=p)) + geom_point(shape=19, color="dodgerblue") +
      geom_hline(yintercept=limit, linetype="dashed", size=2, color="red") + 
      ggtitle("Probability of Jth Observation Being in the Boostrap Sample") + 
      xlab("n (sample size)") + ylab("Probability")


# Observation: as n increases to infinity, the probability approaches the value 1 - e^-1
# which is 0.6321


# part h) Investigate with bootstrap -----------------------------------------------------

# j = 4, n = 100
# Finding numeric probability that the (j = 4th) observation is in the boostrap sample
# Repeatedly create bootstrap samples and each time record whether or not the fourth
# observation is contained in the bootstrap sample. 
set.seed(1)
numReps <- 10000
sampleSize <- 100 # this is n
j = 4
store <- rep(NA, numReps)

for(i in 1:numReps) {
      bootSampleObservations = sample(1:sampleSize, replace=TRUE)
      # count number of times the 4th observation is in the bootstrap sample, using sum()
      # the probability is made of yes/no, so record whether or not this value is > 0
      store[i] = sum(bootSampleObservations == j) > 0
}
mean(store)
# 0.6408
probability(sampleSize)
# 0.6339677
# Close to the true probability value
coins <- c("H", "T")

probOneHead <- 0.5
numTosses <- c(11, 101, 1001, 100001)
allProbHeadsSim <- c()

for (toss in numTosses){
      simulation <- sample(x = coins, size = numTosses, replace=TRUE, 
                           prob=c(probOneHead, 1 - probOneHead))
      probHeadSim <- sum(simulation == "H") / toss 
      allProbHeadsSim <- c(allProbHeadsSim, probHeadSim)
}
print(allProbHeadsSim)


#simulation <- sample(x = coins, size = numTosses, replace=TRUE, 
#                     prob=c(prob.Head, 1 - prob.Head))
#simulation
#table(simulation)
## got 8 heads and 3 tails of 11 tosses.
#table(simulation) / numTosses

#simulation == "H"
#sum(simulation == "H")
#sum(simulation == "T")

############### FalsePositive.R

# Suppose that there is a probability of 10^−3 that any individual has
# a certain disease.
# A test is developed for the disease and the probability that a
# person with disease will test positive is 0.95, while the probability
# that person without the disease will test positive is 0.1.
# Given that a test is positive what is the probability that individual
# has the disease?
# Let D denote the event that \certain individual has the disease"
# R denote the event that \the test result is positive". We want
# to calculate P(D j R).

# ANSWER
# P(D | P) = P(D) * P(P | D) / P(P) 
# = (0.001*0.95) / (0.001*0.95 + 0.999*0.1)
# = 0.0094199

# population of size 1 million 
populationSize =  1000000  
# probability of an individual having the disease
probDisease  = .001
#probability of an individual having the disease testing positive 
probPosGivenDisease = .95             
# probability of an individual not having the disease testing positive. 
probPosGivenNoDisease = .10             

#### First the simulated population is created: 
popSimulation = sample(c("Disease","NoDisease"), size=populationSize, replace=TRUE, 
              prob=c(probDisease, 1 - probDisease))
#popSimulation #

popSimulation[1:60]                ## check the first 60 "people"
sum(popSimulation == "Disease")   ## count how many people with the disease 

#### We now apply the test to the simulated population. 

test = function(x) {
      if (x == "Disease")
            # simulate to have #positives with probability of P(Pos | D) and have
            # the #negatives with probability of P(Neg | D)
            sample(c("Pos","Neg"), size=1, 
                   prob=c(probPosGivenDisease,1 - probPosGivenDisease))
      else # if no disease then ...
            # simulate 1 person to have #positives with  prob P(Pos | No D) and have
            # the #negatives with probability of P(Neg | No Disease)
            sample(c("Pos","Neg"), size=1, 
                   prob=c(probPosGivenNoDisease,1 - probPosGivenNoDisease))
}

####  Now apply the test to the population: 

popSim.test = mapply(test, popSimulation, USE.NAMES=FALSE)
popSim.test[1:40]

popSim.pos = popSimulation[popSim.test == "Pos"]   ##  the number testing positive 


# Calculating, given the positive array above, what is probability of Disease D?  
# Very close to real answer of 0.0094
print(sum(popSim.pos == "Disease")/length(popSim.pos))

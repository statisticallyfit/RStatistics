
# P(Know) = p
# P(NOT KNOW) = q
# P(NOT Correct) = 1 - 1/m
# P(Correct | Know) = 1
# P(Correct | NOT Know) = 1 /m 

#P(Correct) = P(K and C) + P(notK and C)
# P(Correct) = P(K) * P(C|K) + P(notK) * P(C | notK)
# P(Correct) = p*1 + q * 1/m = (mp + q)/m



# 0 means does not know correct answer with probability (1-p = q)
# 1 means knows correct answer with probability p 
# replace = T

# Finding P(C | K)
prob.Know <- 0.6
n <- 100 # samples ize (sample of 100 students
student.sample <- sample(c(0, 1), size=n, replace=TRUE, 
                         prob=c(1 - prob.Know, prob.Know ))
student.sample
sim.prob.Correct.GivenKnow <- sum(student.sample == 1) / n
sim.prob.Correct.GivenKnow


# now finding P(C | notK)
prob.NotKnow <- 1 - prob.Know
student.sample <- sample(c(0, 1), size=n, replace=TRUE, 
                         prob=c(prob.Know, prob.NotKnow))
sim.prob.Correct.Given.NotKnow <- sum(student.sample == 1) / n
sim.prob.Correct.Given.NotKnow

# so now P(C) = P(K)*P(C|K) and P(notK)*P(C|notK)
sim.prob.Correct = prob.Know * sim.prob.Correct.GivenKnow + 
      prob.NotKnow * sim.prob.Correct.Given.NotKnow
sim.prob.Correct


# theoretical answer
# P(C) = (mp + q)/m = (4 * 0.60 + 0.4) / 4 = 0.70




# SOLUTION PRAC
# simulate 10000 trials of 100 students
# simsum is a variable which will store the number of 
# correct answers out of a 100 for 10000 samples
numCorrect<-0 
# set parameter values
probKnow <-0.6 # prob that student knows answer = P(K) = p
N <- 10000 # sample size of students
m <- 4

simSize = 10000

probTheoryCorrect = probKnow*1 + (1-probKnow)/m
listTheoryProbCorrects = replicate(simSize, probTheoryCorrect)
listProbCorrects <- 0


for (i in 1:simSize){
      # generate sample of N students who KNOW THE CORRECT ANSWER = numKnow
      student.sample<-sample(c(0,1),size=N, replace = T,
                        prob=c((1-probKnow),probKnow))
      numKnow <- sum(student.sample) # total number who know the answer
      
      probCorrectGivenNotKnow <- 1/m # P(C | not Know) = 1/m = 1/4
      guess.sample<-sample(c(0,1),size= N - numKnow, replace = T,
                           prob=c((1-probCorrectGivenNotKnow),
                                  probCorrectGivenNotKnow))
      numCorrectGivenNotKnow <- sum(guess.sample) # total number who guessed correctly
      
      numCorrect[i] <- numKnow + numCorrectGivenNotKnow
      
      #probCorrectGivenKnow = 1
      probCorrect = (numKnow + numCorrectGivenNotKnow) / N 
      #probTheoryCorrect = probKnow*1 + (1-probKnow)/m
      listProbCorrects[i] <- probCorrect
}
hist(numCorrect)

#cbind(listTheoryProbCorrects, listProbCorrects)
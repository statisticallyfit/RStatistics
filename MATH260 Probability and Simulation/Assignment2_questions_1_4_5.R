
# Question 1
N <- 10^4
dice1 <- sample(1:6, size=N, replace=TRUE, prob=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
dice2 <- sample(1:6, size=N, replace=TRUE, prob=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
dice3 <- sample(1:6, size=N, replace=TRUE, prob=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

diceSum <- dice1 + dice2 + dice3

# part a) Probability that sum of face values < 10
probLessTen <- sum(diceSum < 10) / N; probLessTen



# part b) probability that face values of the 3 dice are all different
rolls <- cbind(dice1, dice2, dice3)


count.1 <- 0
for (i in 1:N){
      
      # if all of the rolls of the 3 dice are not the same for this iteration i,
      if(rolls[i, 1] != rolls[i,2] && rolls[i,1] != rolls[i, 3] && 
         rolls[i,2] != rolls[i,3]){
            # then we do increment the count
            count.1 = count.1 + 1 
      }
}
# the probability we are finding is: 
probFaceValuesAllDifferent.1 <- count.1 / N; 
probFaceValuesAllDifferent.1



# Another way to do this is with the built-in function 'unique'
count.2 <- 0
for (i in 1:N){
      
      # if all of the rolls for this row i are unique, 
      if(length(unique(rolls[i, ])) == 3){
            # then we increment the count
            count.2 = count.2 + 1 
      }
}
probFaceValuesAllDifferent.2 <- count.2 / N
probFaceValuesAllDifferent.2


# Test to make sure: Should be true, the two methods yield the same answer. 
count.1 == count.2





# Question 4

# The probability that not all passengers cannot be seated is: P(X > 100)
# method 1 of calculating: 
1 - pbinom(100, size=105, prob = 0.90)
# method 2 of calculating 
pbinom(100, size = 105, prob = 0.90, lower.tail=FALSE)



# Quuestion 5: 

# part b) Probability of getting at least 12 phone calls per hour: P(X >= 12)
# method 1
p = ppois(11, lambda=10, lower.tail = FALSE); p
# method 2
1 - ppois(11, lambda=10)

# part c) Probability of getting at least 3 hours of 8 hours with at least
# 12 phone calls. P(N >= 3)
# method 1
pbinom(2, size=8, prob=p, lower.tail = FALSE)
# method 2
1 - pbinom(2, size=8, prob=p)




# Question 6

# part (i)   p = P(X > 2)
# note: q = 1 is the number of failures before success occurs. 
# Method 1
p = pgeom(q=1, prob=0.8, lower.tail = FALSE); p
# Method 2
1 - pgeom(1, prob=0.8)

# part ii): Probability at least 4 points that required at least 2shots
# to win the first point.  P(N >= 4)
# method 1
pbinom(3, size=10, prob = p, lower.tail=FALSE)
# Method 2
1 - pbinom(3, size=10, prob=p)

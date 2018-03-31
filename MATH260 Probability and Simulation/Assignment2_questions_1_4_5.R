
# Question 1
N <- 10^4
dice1 <- sample(1:6, size=N, replace=TRUE, prob=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
dice2 <- sample(1:6, size=N, replace=TRUE, prob=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
dice3 <- sample(1:6, size=N, replace=TRUE, prob=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))

diceSum <- dice1 + dice2 + dice3

# part a) Probability that sum of face values < 10
probLessTen <- sum(diceSum < 10) / N; probLessTen



# part b) probability face values of the 3 dice are all different
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

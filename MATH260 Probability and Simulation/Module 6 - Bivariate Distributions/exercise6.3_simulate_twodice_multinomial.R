
# X = number of 5's that result in a throw of 2 dice, x = {0,1,2}
# Y = number of 6's that result in a throw of 2 dice, y = {0,1,2}
#
sampleSize <- 10^4
die1 <- sample(x=1:6, size=sampleSize, replace=TRUE) # each prob = 1/6
die2 <- sample(x=1:6, size=sampleSize, replace=TRUE) # each prob = 1/6

# X and Y are independent so P(X, Y) = P(X)P(Y)
table(die1, die2)
jointProb <- table(die1, die2)/sampleSize
jointProb
jointProb[5:6] # this is the joint probability of

# method 1
prob56 <- diag(table(die1, die2))[5:6]/sampleSize

# method 2
fives <- (die1 == 5) + (die2==5) 
sixes <- (die1 == 6) + (die2 == 6)
jointProb56 <- table(fives, sixes)/sampleSize
jointProb56 
# now the probs for (5,5) and (6,6) are
jointProb56[3,1]
jointProb56[1,3]

(2/3)^2

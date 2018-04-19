

# part a)

# THEORETICAL ANSWER
n = 5 # sample of 5 crocodiles, prob males = 2, females = 1
dmultinom(x=c(2, 1, 2), size=n, prob=c(0.3, 0.15, 1-0.3-0.15))

# SIMULATION ANSWER
nSim <- 10^4

X2_Y1 <- 0
diffRandVar <- array(0, nSim)
names = c("M", "F", "O")
counts <- array(0, 3) # holds observed counts of M, F, O

for(i in 1:nSim){ # we need several tries in order to have counts add up. 
      crocs <- sample(x=names, size=n, prob=c(0.3, 0.15, 0.55), replace=TRUE)      
      
      for(j in 1:3){
            counts[j] = sum(crocs == names[j])
      }
      
      # if males = 2, and females = 1
      if(counts[1] == 2 && counts[2] == 1){
            X2_Y1 <- X2_Y1 + 1
      }
      diffRandVar[i] <- counts[1] - counts[2]
}

counts
head(diffRandVar)
X2_Y1 

# part a) ---------------------------------------------------------
prob_X2_Y1 = X2_Y1 / nSim
prob_X2_Y1 # theoretical ans: 0.1225125

# part b) ---------------------------------------------------------
# expectation E(X - Y)
mean(diffRandVar) # theoretical ans: 0.75

# part c)
var(diffRandVar) # theoretical ans: 2.1375

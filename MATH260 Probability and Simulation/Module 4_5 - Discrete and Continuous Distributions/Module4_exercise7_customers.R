

N <- 1000

# Poisson lambda = 2
lambda = 2 # 2 customers per half minute

# Probability 5 or more customers in half minute interval 
# These are the same: 
p = ppois(5-1, lambda=lambda, lower.tail=FALSE); p
1 - ppois(4, lambda=lambda)

# Simulating arrival of 100 customers in the half minute intervals, estimate P(X >= 5)
customers <- rpois(n = N, lambda=lambda)
customers[10] # number of customers arriving in the tenth half minute interval
mean(customers) # close to 2
hist(customers)

p.sim <- sum(customers >= 5) / N; p.sim
p

# There are 3 intervals here where at least 5 customers entered
sim.results <- table(customers); sim.results
data.frame(PSim=p.sim, P=p)

# Unlikely P(X >= 5) can suspect that mu = 2 may be greater than 2
# if indeed we think five or more customers arrive in a half minute interval
# in our observation. 

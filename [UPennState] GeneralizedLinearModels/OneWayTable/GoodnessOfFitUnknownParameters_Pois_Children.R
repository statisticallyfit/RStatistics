source("/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R")

##################################################################################
## MEthod for Unknown parameters
## (1) estimate parameter θ by efficient method (MLE)
## (2) find estimated probabilities
## (3) find estimated expected values
## (4) find GOF statistics (X2 and G2)

# df = number of unknown parameters under HA - number of unknown parameters under H0
# df = (k − 1) − d, where d = dim(θ) = number of parameters in θ (MLE estimate).
##################################################################################


# input data 
kids <- c(0, 1, 2, 3) # going to attach 4+ category later 
obs <- c(19, 26, 29, 13, 13); obs  # 13 families for 4+ children category
n <- sum(obs); n 

# Estimated lambda, mean of poisson distribution
# here is where we assume that level 4, 5 are 10, 3 respectively
lambda.hat <- sum(0*19, 1*26, 2*29, 3*13, 4*10, 5*3) / n; lambda.hat

# Estimated expected probabilities
pi.hat <- dpois(x=kids, lambda = lambda.hat); pi.hat
# now attach probability for 4+ cell
pi.hat <- c(pi.hat, 1 - sum(pi.hat)); pi.hat

# Estimated expected values
exp.hat <- n * pi.hat; exp.hat 

# Chi-Square GOF test - manual ------------------------
X2 <- sum((obs - exp.hat)^2/exp.hat); X2
# calculate df: 
### d = dim(θ)  = num parameters of θ (which is lambda) = 1
### k = numcells - 1 = 5 - 1 = 4
### df = k - d = 4 - 1 = 3 
df <- 3
1 - pchisq(X2, df)

# CHI-Square GOF test - automatic 
chi.test <- chisq.test(x=obs, p=pi.hat); chi.test

# Likelihood Ratio Test - manual ----------------------
G2 <- 2 * sum(obs * log(obs / exp.hat)); G2
1 - pchisq(G2, df=3)

# Likelihood Ratio Test - automatic -------------------
likelihoodRatioTest(obs, exp.hat) # but df and thus p-value are incorrect

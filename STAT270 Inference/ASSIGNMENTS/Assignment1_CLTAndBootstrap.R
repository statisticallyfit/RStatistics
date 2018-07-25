setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT270 Inference/ASSIGNMENTS")

# Question 1

# part c) Probability X-bar is within 0.2 of true mean. 
diff(pnorm(c(-sqrt(2), sqrt(2))))
# [1] 0.8427008


# part d) probability X-bar is within 0.05 of true mean with probability 0.80,
# now we find the sample size n, so find the quantile z
qnorm((1-0.80)/2)
# [1] -1.281552



# Question 2 ---------------------------------------------------------------------

# part a) probability difference of mean gpa A and mean gpa B is greater than 0.5
pnorm((0.5-(5-4.8))/sqrt(2*0.25^2/9), lower.tail = F)
#[1] 0.005454749

# part b) probability that mean gpa B > mean gpa A
pnorm((0-(5 - 4.8))/sqrt(2*0.25^2/9))
#[1] 0.04484301



# part c) simulate sample mean for 9 students from each school

# General function: returns a vector of length R where each entry is the
# mean of n data points drawn from the Normal(mu, sigma^2) distribution. 
# n = sample size to take (number of iid random variables)
# R = number of replications = number of times we draw the n samples. 
simulate.norm = function(n, R, mu, sigma) {
      x_bar = rep(NA, R) # create empty vector first with length R = num replicates.
      
      for(i in 1:R) {
            # data vector holds the n iid random samples so this would be 
            # equivalent to the data A1, A2, .. A9 above when drawing from
            # the Normal distribution of School A gpa's.
            data = rnorm(n=n, mean=mu, sd=sigma)
            x_bar[i] = mean(data)
      }
      # Comparing the simulated mean and standard deviation with
      # the theoretical ones.
      cat("sim.mean = ", mean(x_bar), "\ttheoretical.mean =", mu,"\n")
      cat("sim.sd = ", sd(x_bar), "\ttheoretical.sd =", sigma/sqrt(n),"\n")
      
      return(invisible(x_bar))
}
## Repeat the experiment above with N = 1000 times and oberserve the mean 
N = 10000

X_bar_A = simulate.norm(n=9, R=N, mu=5, sigma=0.25)
hist(X_bar_A, main="Sample means of GPA for School A")

X_bar_B = simulate.norm(n=9, R=N, mu=4.8, sigma=0.25)
hist(X_bar_B, main="Sample means of GPA for School B")


# Estimate the probability for a)
# P(X_bar_A >= X_bar_B + 0.5)
sum(X_bar_A - X_bar_B >= 0.5) / N
# [1] 0.0051
# this is close to result 0.00545 from part a)

# Estimate the probability for b)
# P(X_bar_B > X_bar_A)
sum(X_bar_B > X_bar_A) / N
# 1] 0.0433
# this is close to result of 0.0448 from part b)



# Question 3 ---------------------------------------------------------------------

movieData = scan("movies.txt")

# part a) find mean and standard deviation of the movie running times

n = length(movieData); n # length
# [1] 20
xbar = mean(movieData); xbar # the mean running time
# [1] 104.45
s = sd(movieData); s # the standard deviation
# [1] 13.60524


# part b)
t.crit = abs(qt((1-0.95)/2, df=n-1)); t.crit
# 2.093024
CI = c(xbar - t.crit* s / sqrt(n), xbar + t.crit*s/sqrt(n))
CI
# [1]  98.48735 110.41265


# part c)

bootstrapMean <- function(data, R, level=0.95){
      # Get n samples from the known population distribution. (this is the data given)
      n = length(data) # observed data, original data
      
      # Sample from F-hat (instead of rexp) to sample with replacement from F-hat
      H.star = rep(NA, R)
      for(i in 1:R){
            # resample from original data with replacement. 
            dataStar = sample(data, size=n, replace=TRUE) #this is the bootstrap sample
            H.star[i] = mean(dataStar)
      }
      
      # MEAN(H*) should be close to MU_XBAR should be close to MU
      # SD(H*) should be close to SD_XBAR should be close to  S/SQRT(N)
      cat("boot estimate of sampling mean = ", mean(H.star), "\n", sep="")
      cat("xbar (mean of data) = ", mean(data), "\n\n", sep="")
      
      B.hat = mean(H.star) - mean(data) # estimated bias
      cat("B.hat = mean(hstar) - mean(data) = ", B.hat, "\n", sep="")
      mu.hat.B = mean(data) - B.hat  # bias-corrected version of sample mean
      cat("mu.hat.B = mean(data) - B.hat = ", mu.hat.B, "\n\n",sep="")
      
      se.mean = sd(H.star) # standard error of the mean by bootstrap
      cat("boot estimate of se.mean = ", se.mean, "\n", sep="") 
      cat("se.mean from data = s / sqrt(n) = ", sd(data)/sqrt(n), "\n\n", sep="")
      
      # 95 % confidence interval methods. 
      # calculate critical value first
      lower = abs( (1-level)/2)
      upper = lower + level 
      z.crit = abs(qnorm(lower))
      
      
      cat("CI by normal approximation: ")
      cat("(",mu.hat.B - z.crit*se.mean,", ",mu.hat.B + z.crit*se.mean,")","\n",sep="")
      
      cat("CI by percentile method: ")
      qs = quantile(H.star, c(lower, upper))
      cat("(", qs[1],", ", qs[2],")", "\n", sep="") 
      
      cat("CI by basic method:", "")
      cat("(", 2*xbar - qs[2],", ", 2*xbar - qs[1],")", "\n", sep="")
      
      return(invisible(H.star))
}

set.seed(123)
bootstrapMean(data=movieData, R = 10000)

### OUTPUT
# boot estimate of sampling mean = 104.4521
# xbar (mean of data) = 104.45
# 
# B.hat = mean(hstar) - mean(data) = 0.002105
# mu.hat.B = mean(data) - B.hat = 104.4479
# 
# boot estimate of se.mean = 2.952608
# se.mean from data = s / sqrt(n) = 3.042225
# 
# CI by normal approximation: (98.66089, 110.2349)
# CI by percentile method: (98.8, 110.3)
# CI by basic method: (98.6, 110.1)




# part d) calculate 95% basic bootstrap Ci for the mean
# from part b), we see from printout of the function that the basic method yields
# a CI = (98.6, 110.1)



# part e)
bootstrapStandardDeviation <- function(data, R, level=0.95){
      # Get n samples from the known population distribution. (this is the data given)
      n = length(data) # observed data, original data
      
      # Sample from F-hat (instead of rexp) to sample with replacement from F-hat
      H.star = rep(NA, R)
      for(i in 1:R){
            # resample from original data with replacement. 
            dataStar = sample(data, size=n, replace=TRUE) #this is the bootstrap sample
            H.star[i] = sd(dataStar)
      }
      
      cat("boot estimate of standard deviation of movie times = ", mean(H.star), "\n", sep="")
      cat("sd (sd of data) = ", sd(data), "\n\n", sep="")
      
      B.hat = mean(H.star) - sd(data) # estimated bias
      cat("B.hat = mean(hstar) - sd(data) = ", B.hat, "\n", sep="")
      sd.hat.B = sd(data) - B.hat  # bias-corrected version of sample standard deviation
      cat("sd.hat.B = sd(data) - B.hat = ", sd.hat.B, "\n",sep="")
      
      # this is the sample standard deviation of the generated Hstar data,
      # so is the sample std.dev of the sampled standard deviations (Hstars)
      cat("sd(H.star) = ", sd(H.star), "\n\n",sep="")
      
      
      
      cat("CI by percentile method: ")
      qs = quantile(H.star, c(lower, upper))
      cat("(", qs[1],", ", qs[2],")", "\n", sep="") 
      
      cat("CI by basic method:", "") 
      # calculate critical value first
      lower = abs( (1-level)/2)
      upper = lower + level 
      z.crit = abs(qnorm(lower))
      cat("(", 2*sd(data) - qs[2],", ", 2*sd(data) - qs[1],")", "\n", sep="")
      
      return(invisible(H.star))
}

set.seed(123)
bootstrapStandardDeviation(movieData, R=10000)

## OUTPUT:

# boot estimate of standard deviation of movie times = 13.13766
# sd (sd of data) = 13.60524
# 
# B.hat = mean(hstar) - sd(data) = -0.4675822
# sd.hat.B = sd(data) - B.hat = 14.07282
# sd(H.star) = 1.888106
# 
# CI by percentile method: (9.366215, 16.75179)
# CI by basic method: (10.45869, 17.84427)
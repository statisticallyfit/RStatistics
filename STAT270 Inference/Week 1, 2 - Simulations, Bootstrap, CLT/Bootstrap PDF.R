
# First simulate data from exponential distribution (so 10 random variables
# and each with rate parameter 0.5)
#theta = 2
#N = 10
#sampleexp <- rexp(n=N, rate = 1 / theta)
#sample.mean <- mean(sampleexp); mean.sample
#sample.sd1 <- theta / N^(1/2); sample.sd1 # var(X) = theta^2, and var(sampleX) = var(X)/N
## But pdf says to use (s) instead of (theta)
#sample.sd <- sd(sampleexp) / N^(1/2); sample.sd
#hist(sampleexp)


# Small sample confidence interval: 
# mu.pop = mu.sample +/- 2 * s/sqrt(n)
#mu.pop.CI = c(mean.sample - 2*sd.sample, mean.sample + 2*sd.sample)
#mu.pop.CI
 
# teacher got (2.3 - 1.6, 2.3+1.6) = (0.7, 3.9)







# RESIMULATING (my word, not bootstrapping)
# n = sample size (so there would be X1, ... Xn iid random vars sampled)
# theta = the parameter of the exponential distribution
# R = number of replications

resimulate.exp <- function(theta, n, R){
      T.star = rep(NA, R)  # the vector holding estimates of the mean of
      # each sampled dataset
      
      # Generate samples from the KNOWN population dist: Exp(theta=2)
      for(i in 1:R){
            # resampling from population, not from sample. 
            sampleExp = rexp(n=n, rate= 1 / theta)
            T.star[i] = mean(sampleExp)
      }
      
      cat("mu_xbar (tstar) = ", mean(T.star), "\n", sep="")
      cat("mu_xbar = ", theta, "\n", sep="")
      cat("mu = ", theta, "\n\n", sep="")
      
      cat("sigma_xbar (tstar) = ", sd(T.star), "\n", sep="")
      cat("sigma_xbar = sigma/sqrtn(n) = ", theta/sqrt(n), "\n", sep="")
      cat("sigma_xbar = s / sqrt(n) = ", sd(sampleExp)/sqrt(n), "\n\n", sep="")
      
      print(quantile(T.star, c(0.025, 0.50, 0.975))) # 95% CI interval (0.96, 3.4)
      
      return(invisible(T.star))
}

set.seed(2463)
resimulate.exp(theta=2, n=10, R=10000)





# Actual bootstrapping: 
#     1) generate samples from the UNKNOWN F-hat distribution. Resample n points
#     from the original data with replacement, calculate the parameter (mean)
#     of the sample to estimate true population mean. 
#
#     2) Repeat R times. T1*, T2*, T3*, ... TR*
#
#     3) find the mu_x-bar and sd_x-bar from the T* samples. (Getting the sampling
#     distribution parameters of the T-stars) since 
#     mu.x-bar ~ mean(Tstars) and
#     sd.x-bar ~ sd(Tstars)
#     
#     4) use the dist to come up with reasonable estimator and CI using the
#     percentile method OR (Normal bootstrap CI: x-bar +/- 2*sd_x-bar) OR
#     CLT-based CI: x-bar +/- 2s/sqrt(n)

bootstrapMean.exp <- function(theta, n, R){
      # Get n samples from the known population distribution. 
      data = rexp(n=n, rate=1/theta) # observed data, original data. 
      
      # Sample from F-hat (instead of rexp) to sample with replacement from F-hat
      T.star = rep(NA, R)
      for(i in 1:R){
            # resample from original data with replacement. 
            dataStar = sample(data, size=n, replace=TRUE)
            T.star[i] = mean(dataStar)
      }
      
      # MEAN(T*) should be close to MU_XBAR should be close to MU
      # SD(T*) should be close to SD_XBAR should be close to  S/SQRT(N)
      cat("boot.mu_xbar (tstar) = ", mean(T.star), "\n", sep="")
      cat("mu_xbar = ", theta, "\n", sep="")
      xbar = mean(data)
      cat("xbar (mean of data) = ", xbar, "\n", sep="")
      cat("mu = ", theta, "\n\n", sep="")
      
      bias = mean(T.star) - xbar
      cat("bias = mean(tstar) - xbar = ", bias, "\n", sep="")
      mu.biascorrect = xbar - bias 
      cat("bias.correct.est.mu = xbar - bias = ", mu.biascorrect, "\n\n",sep="")
      
      cat("boot.sigma_xbar (tstar) = ", sd(T.star), "\n", sep="")
      cat("sigma_xbar = sigma/sqrtn(n) = ", theta/sqrt(n), "\n", sep="")
      cat("sigma_xbar = s / sqrt(n) = ", sd(data)/sqrt(n), "\n\n", sep="")
      
      # 95 % confidence interval methods. 
      cat("CI by normal approximation: ")
      cat("(",mu.biascorrect - 1.96*sd(T.star),", ",mu.biascorrect + 1.96*sd(T.star),")","\n",sep="")
      
      cat("CI by percentile method: ")
      qs = quantile(T.star, c(0.025, 0.975))
      cat("(", qs[1],",", qs[2],")", "\n") # 95% CI interval (0.96, 3.4)
      
      cat("CI by basic method:", "")
      cat("(", 2*xbar - qs[2],",", 2*xbar - qs[1],")", "\n", sep="")
      
      return(invisible(T.star))
}

set.seed(2463)
bootstrapMean.exp(theta=2, n=10, R=10000)


# data = observed,original data
# R = number replications or number of bootstrap sample statistics to calculate. 
# level = percent (as decimal) for the confidence interval, also called the
# confidence coefficient. 
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
      cat("boot.mu_xbar (hstar) = ", mean(H.star), "\n", sep="")
      cat("xbar (mean of data) = ", mean(data), "\n\n", sep="")
      
      B.hat = mean(H.star) - mean(data) # estimated bias
      cat("B.hat = mean(hstar) - mean(data) = ", B.hat, "\n", sep="")
      mu.hat.B = mean(data) - B.hat  # bias-corrected version of mu
      cat("theta.hat.B = mean(data) - bias = ", mu.hat.B, "\n\n",sep="")
      
      se.mean = sd(H.star) # standard error of the mean
      cat("boot.se.mean (hstar) = ", se.mean, "\n", sep="") # call this se.mean
      #print(sqrt(var(H.star))) # same as sd(H.star)
      cat("se.mean = sigma/sqrtn(n) = ", theta/sqrt(n), "\n", sep="")
      cat("se.mean = s / sqrt(n) = ", sd(data)/sqrt(n), "\n\n", sep="")
      
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




# Example 1: Data list

data = c(8,6,5,10,8,12,9,9,8,11,7,3,6,7,5, 8,10,7,8,8,10,8,5,10,8,6,10,6,8,14)
mean(data)
length(data)

bootstrapMean(data=data, R=9999)


# 1) get R = 1000... etc
# 2) sample with replacement from (x), a bootstrap sample of size n, x*1
# * Calculate the statistic H(x1*) save this value. 
# 3) repeat step 2 with new samples, so a total of R boostrap statistics: 
# H1*, H2*, H3*, ... HR*






# Example 2 ---------------------------------------------------------------------
# Mice example

miceData <- c(94, 197, 16, 38, 99, 141, 23, 52, 104, 146, 10, 51, 30, 40, 27, 46)
n = length(miceData); n

xbar = mean(miceData); xbar
s = sd(miceData) ; s

# Analytical 95% CI for the mean using CLT (use s since n = 16 < 30)
CI = c(xbar - 1.96 * s / sqrt(n), xbar + 1.96 * s / sqrt(n)); CI

# NOTE: if repeated samples were taken and the 95% CI was computed
# for each sample this way, then 95% of the time true mean would be between
# tose two values and 5% of the time it would not.

# INTERPRET: we are 95% confident that mean survival time for mice after
# teh test surgery is between 42.8 and 96.4 days. 

bootstrapMean(data=miceData, R=9999)



# Example 3 - typhoon ------------------------------------------------------

typhoonData <- c(13,7,14,20,13,12,12,15,20,17,11,14,16,12,17,7,14,
                 15,16,20,17,20,15,22,26,25,27,18,23,26)

bootstrapMean(typhoonData, R=9999)

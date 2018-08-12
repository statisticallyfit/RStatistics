source('/datascience/projects/statisticallyfit/github/R/RStatistics/FORMULAS.R', echo=FALSE)

# Question 1

typhoonData <- c(13,7,14,20,13,12,12,15,20,17,11,14,16,12,17,7,14,
                 15,16,20,17,20,15,22,26,25,27,18,23,26)

bootstrapMean(typhoonData, R=9999, level=0.90)



# Question 2
# simulate sample n = 9 data points from normal(mu=5, var=6)
# compute sample mean and 95% CI using the CLT
sampleNormData <- rnorm(n=9, mean=5, sd=sqrt(6))
n = 9
xbar = mean(sampleNormData); xbar
s = sd(sampleNormData)
se.mean = s / sqrt(n); se.mean

CI = c(xbar - 1.96*se.mean, x.bar + 1.96*se.mean); CI
# Yes this covers the true mean since mu = 5 but I got (3.55, 9.46)

# Replicating
R = 1000
theta.hat = rep(NA, R)
CIs.covered = 0
n = 9
mu = 5; sigma = sqrt(6)
L = rep(NA, R)
U = rep(NA, R)

for(i in 1:R){
      sampleNormData <- rnorm(n=n, mean=mu, sd=sigma)
      xbar = mean(sampleNormData)
      se.mean = sd(sampleNormData) / sqrt(n)
      theta.hat[i] = xbar
      
      L[i] = xbar - 1.96*se.mean #
      U[i] = x.bar + 1.96*se.mean #
      
      if((L[i] <=mu) & (U[i] >=mu) ){
            CIs.covered = CIs.covered + 1
      }
}
theta.hat[1:5]
L[1:5]
U[1:5]



# Plot the CI's
library(plotrix)
plotCI(x=1:R, rep(mu, R), ui=U, li=L)
?plotCI

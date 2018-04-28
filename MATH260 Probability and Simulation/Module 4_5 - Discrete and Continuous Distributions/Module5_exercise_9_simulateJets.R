
N <- 1000
randNorm <- rnorm(n=N, mean=101, sd=5) # units in decibels

# part b)
probNoiseGreater110 = sum(randNorm >= 110) / N
probNoiseGreater110


# part e) estimate the value at 95% percentile (position 950)
#randNorm_105 <- rnorm(n=N, mean=105, sd=5)
randNorm <- rnorm(n=N, mean=101, sd=5) # units in decibels
sorted <- sort(randNorm)
estimatedNum95Percentile <- sorted[round(N * 0.95)]
number95Percentile = 105
reductionRequired = abs(estimatedNum95Percentile - number95Percentile)
reductionRequired

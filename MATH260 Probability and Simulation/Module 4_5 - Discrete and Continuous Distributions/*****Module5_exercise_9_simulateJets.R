
N <- 1000
randNorm <- rnorm(n=N, mean=101, sd=5) # units in decibels

# part b)
probNoiseGreater110 = sum(randNorm >= 110) / N
probNoiseGreater110


# part e) estimate the value at 95% percentile (position 950)

sorted <- sort(randNorm)
value95Percentile <- sorted[N * 0.95]
oldMean
reductionRequired = value95Percentile - 

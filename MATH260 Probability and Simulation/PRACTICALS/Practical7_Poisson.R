
# the random variable range is x1 = 0 ... 50 , x2 = 0...50
x1 <- x2 <- seq(0, 50, by=1)

# define parameters
mu1 <- 1; mu2 <- 3

# 2) the joint pmf of X1 and X2
pmf.X1X2 <- (mu1^x1) *(mu2^x2) * exp(-(mu1 + mu2)) / (factorial(x1) * factorial(x2))
pmf.X1X2


# 3) Verify independence
pmf.X1 <- mu1^x1 * exp(-mu1) / factorial(x1)
pmf.X2 <- mu2^x2 * exp(-mu2) / factorial(x2)

# So they are the same: fx * fy = fxy
data.frame(fx1fx2=pmf.X1 * pmf.X2, fx1x2 = pmf.X1X2, 
           difference=round(pmf.X1X2 - pmf.X1*pmf.X2, 20))


# 4) calculate pmf of X1 + X2 using dpois
pmf.dpois.X1 <- dpois(x1, lambda=mu1)
pmf.dpois.X2 <- dpois(x2, lambda=mu2)

# see they are the same
range(pmf.dpois.X1 - pmf.X1)
range(pmf.dpois.X2 - pmf.X2)

# now calculate X1 + X2 directly
pmf.dpois.X1X2 <- dpois(x1 + x2, lambda=mu1 + mu2)
pmf.dpois.X1X2.sep <- dpois(x1, lambda=mu1) + dpois(x2, lambda=mu2)

# see it is not quite the same as the formula equation above
range(pmf.dpois.X1X2 - pmf.dpois.X1X2.sep)



# 5) 
 # plot the difference between them
plotMultipleContinuousDist(x1 + x2, list(pmf.dpois.X1X2 - pmf.dpois.X1X2.sep))

# the difference ranges between 0 to -0.4 and difference is greatest when xs are
# small (x1 + x2 is small)
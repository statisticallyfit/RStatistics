#________UniformMLE.R___________
nsims <- 100
theta = 10 # true parameter
theta.estimates <- numeric(nsims)
for (i in 1:nsims) {
      rn <- runif(n=10, min=0, max=theta)
      theta.estimates[i] <- max(rn)
}
plot(density(theta.estimates))

# unbiased but this MOM estimator has high variance. 
abline(v=theta, lty=1)
abline(v=mean(theta.estimates), lty=2)

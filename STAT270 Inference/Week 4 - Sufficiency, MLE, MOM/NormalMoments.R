#________NormalMoments.R___________
R <- 1000
n <- 10
mu.estimates <- numeric(R)
var.estimates <- numeric(R)
var2.estimates <- numeric(R)
for (i in 1:R) {
      normalData <- rnorm(n=n, mean=14, sd=4)
      mu.estimates[i] <- mean(normalData)
      var.estimates[i] <- mean((normalData - mean(normalData))^2 )
      var2.estimates[i] <- (var.estimates[i])*n/(n-1)
}

plot(density(mu.estimates))
abline(v=14)
abline(v = mean(mu.estimates), lty=2)

plot(density(var.estimates), xlim=c(0,40))
abline(v= mean(var.estimates), lty=2)
abline(v=16)

plot(density(var2.estimates), xlim=c(0,40))
abline(v= mean(var2.estimates), lty=2)
abline(v=16)

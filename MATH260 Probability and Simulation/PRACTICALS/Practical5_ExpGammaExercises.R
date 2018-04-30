
# Question 1


## Create density function values
x <- seq(0, 10, by=0.01)
gamma1 <- dgamma(x=x, shape=2, scale=1)
gamma2 <- dgamma(x, shape=3, scale=1)
gamma3 <- dgamma(x, shape=4, scale=1)
gsList <- list(gamma1, gamma2, gamma3)

plotMultipleContinuousDist(x, gsList)


# with base R
leg.txt <- c(expression(paste("Gamma","(",alpha, "=","2",",",beta,"=",1,")")),
             expression(paste("Gamma","(",alpha, "=","3",",",beta,"=",1,")")),
             expression(paste("Gamma","(",alpha, "=","4",",",beta,"=",1,")")))
# Plot results
plot(x, gamma1, type="l", ylim=c(0,0.4), ylab="Density", main="Gamma Densities",las =1)
lines(x, gamma2, col=2)
lines(x, gamma3, col=3)
legend(x=5.5, y=0.3, legend=leg.txt, fill=c(1,2,3))


# As the shape parameter alpha is increased, the curve becomes more centered
# from the original right skewed shape. 




# Question 2
# Exponential dist is Gamma(1, BETA), BETA > 0. 


# Question 3
randGamma <- rgamma(1000, shape=2, scale=3) # alpha=2, beta=3
mean(randGamma) # E(X) = a*b = 6
var(randGamma) # V(X) = a*b^2 = 2*9 = 18

xs <- seq(from=-3, to=3, by=0.1)
ys1 <- dnorm(x=xs, mean=0, sd = 0.5)
ys2 <- dnorm(x=xs, mean=0, sd = 1)
ys3 <- dnorm(x=xs, mean=0, sd = 2)

plotMultipleContinuousDist(xs, list(ys1, ys2, ys3))



# part a) monte carlo
# X ~ Unif(0, 1), with f(x) = 1 for x = [0,1]. If g(x) = e^x then 
# find E(g(X))
n <- 10000
x <- runif(n=n, min=0, max=1)
gx <- exp(x)
mean(gx)
estimateOfE = mean(gx) + 1
estimateOfE

integrate(function(xx) exp(xx), lower=0, upper=1)



# part c)
# h(x) = 1/(1 + x^2). Find E(h(X))
n <- 10000
x <- runif(n=n, min=0, max=1)
hx <- 1 / (1 + x^2)
mean(hx) # this is arctan(1) = pi/4 analytically so mutliply by 4 to get pi estimate
mean(hx)*4





# Multiple simulations
simE <- 0 # store estimates of E
simPi <- 0 # store estimates of pi

# num samples
n <- 1000

for(i in 1:1000){
      # gnerate sample of size n from Unif(0,1)
      us <- runif(n, min=0, max=1)
      
      EeX <- (1/n)*sum(exp(us)) 
      e <- 1 + EeX
      simE[i] = e
      
      # find E[1 / (1+x^2)]
      EhX <- (1/n) * sum(1 / (1 + us^2))
      # hx = arctan(1) = pi/4
      pie <- EhX * 4
      simPi[i] <- pie
}

# Plotting the estimated values (n each of E and Pi)
df <- data.frame(simE=simE, simPi=simPi)
df.melt <- melt(df)
ggplot(df.melt, aes(x=value, colour=variable)) + geom_density(size=1)

# mean estimates
mean(simPi)
mean(simE)

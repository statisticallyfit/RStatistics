



# General function: returns a vector of length R where each entry is the
# mean of n data points drawn from the Exp(beta) distribution. 
      # n = sample size to take (number of iid random variables)
      # R = number of replications = number of times we draw the n samples. 
      # beta = parameter of exp distribution. 
simulate.exp = function(n, R, beta) {
      x_bar = rep(NA, R)
      
      for(i in 1:R) {
            x = rexp(n, rate=1/beta)
            x_bar[i] = mean(x)
      }
      hist(x_bar)
      #return(x_bar)
      cat("sim.mean = ", mean(x_bar), "\ttheoretical.mean =", beta,"\n")
      cat("variance = ", var(x_bar), "\ttheoretical.var =", beta^2/n,"\n")
}



# Suppose we have X1, X2, ... Xn from Exp(2) dist (mean = beta = 2, var = beta^2 = 4)
# Each X_i = Exp(2)
# Then X_bar dist = Normal(mean=beta, var = beta^2/n), for n >= 30 



# 1) simulate a mean of n = 3 data points from Exp(2)
set.seed(5234)
beta = 2
expData <- rexp(n=3, rate=1/beta)
expData
x_bar = mean(expData); x_bar

# 2) repeat R = 1000 times
R = 1000
x_bar = rep(NA, R)
for(i in 1:R){
      data = rexp(n=3, rate=1/beta)
      x_bar[i] = mean(data)
}
summary(x_bar)
mean(x_bar) # theoretical mean = 2
var(x_bar) # theoretical mean = beta^2/n = 4  / 3 = 1.333333...

# 3) plot histogram to see how normal it is
hist(x_bar)

# customize the plot
upper.x = max(x_bar) + 1
hist(x_bar, xlab="Mean of 3 data points (n = 3)", freq=F, main="", 
     breaks=seq(0, upper.x, by=0.2))




simulate.exp(n=3, R=10, beta=2)

simulate.exp(n=3, R=1000, beta=2) # lower n = 3 is less normal than higher n = 50 > 30
simulate.exp(n=50, R=1000, beta=2)
simulate.exp(n=1000, R=1000, beta=2)

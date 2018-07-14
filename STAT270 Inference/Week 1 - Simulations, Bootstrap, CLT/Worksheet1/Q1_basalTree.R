

# part b)  write function to return vector of length R where each entry is the 
# mean of n data points drawn from normal N(mu, sigma^2) distribution

# General function: returns a vector of length R where each entry is the
# mean of n data points drawn from the Norm(mu, sigma^2) distribution. 
      # n = sample size to take (number of iid random variables)
      # R = number of replications = number of times we draw the n samples. 
simulate.norm = function(n, R, mu, sigma) {
      x_bar = rep(NA, R)
      
      for(i in 1:R) {
            x = rnorm(n=n, mean=mu, sd=sigma)
            x_bar[i] = mean(x)
      }
      hist(x_bar)
      #return(x_bar)
      cat("sim.mean = ", mean(x_bar), "\ttheoretical.mean =", mu,"\n")
      cat("variance = ", var(x_bar), "\ttheoretical.var =", sigma^2/n,"\n")
      
      return(invisible(x_bar))
}


# part c) simulate n = 9, R = 10,000 mu = 3, sigma=4 ---------------------------
x_bar = simulate.norm(n=9, R=10000, mu=3, sigma=4)

# calc P(|X_bar - mu| <= 2) = P(1 <= X_bar <= 5)
sum(x_bar <= 5 & x_bar >= 1)/length(x_bar) # close to theoretical!

diff(pnorm(c(1, 5), mean=3, sd=4/sqrt(9)))

# theoretical: P(1 <= x_bar <= 5)
z1 = (1-3)/(4/sqrt(9)); z1
z2 = (5-3)/(4/sqrt(9)); z2
diff(pnorm(c(z1, z2), mean=0, sd=1))



# part d) ------------------------------------------------------------------------

simulate.norm(n=3, R=10000, mu=3, sigma=5)
simulate.norm(n=100, R=10000, mu=3, sigma=5)
simulate.norm(n=1000, R=10000, mu=3, sigma=5)
# note: for larger n, the std sigma/sqrt(n) gets smaller and smaller. 
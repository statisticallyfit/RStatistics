# simulates the distribution of the maximum observed value (instead of the x-bar from 
# exp and norm functions) for n = 10 observations from Gamma(alpha=2, beta=1/2)

simulate.gammaMax = function(n, R, alpha, beta) {
      x_max = rep(NA, R)
      
      for(i in 1:R) {
            x = rgamma(n=n, shape=alpha, scale=beta)
            x_max[i] = max(x)
      }
      hist(x_max)
      
      cat("sim.max = ", mean(x_max), "\n") #, "\ttheoretical.mean =", mu,"\n") ???
      
      return(invisible(x_max))
}


simulate.gammaMax(n=10, R=10000, alpha=2, beta=1/2) # this is chisquare df = 1

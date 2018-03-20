## MaxBalls.R

set.seed() 
maxball <- numeric(1000)  # vector to hold data

for ( i in 1:1000){       # 1000 simulations
      draw <- sample(1:5,replace=F,size=2) # draw sample,
      maxball[i] <- max(draw)              # find max
}


hist(maxball)  #plot the histogram
plot(ecdf(maxball)) # ecdf = empirical cdf

mean(maxball) # this is the monte carlo estimate of the mean

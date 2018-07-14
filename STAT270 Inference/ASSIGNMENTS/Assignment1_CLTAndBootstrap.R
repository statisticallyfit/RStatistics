# Question 1

# part c) Probability X-bar is within 0.2 of true mean. 
diff(pnorm(c(-sqrt(2), sqrt(2))))
# [1] 0.8427008


# part d) probability X-bar is within 0.05 of true mean with probability 0.80,
# now we find the sample size n, so find the quantile z
qnorm((1-0.80)/2)
# [1] -1.281552



# Question 2 ---------------------------------------------------------------------

# part a) probability difference of mean gpa A and mean gpa B is greater than 0.5
pnorm((0.5-(5-4.8))/sqrt(2*0.25^2/9), lower.tail = F)
#[1] 0.005454749

# part b) probability that mean gpa B > mean gpa A
pnorm((0-(5 - 4.8))/sqrt(2*0.25^2/9))
#[1] 0.04484301



# part c) simulate sample mean for 9 students from each school

# METHOD 1 --------------------

## Repeat the experiment above with N = 1000 times and oberserve the mean 
N = 10000

## create vectors with size N to store values. These vectors represent the random 
# sample of size 9. A1 to A9 represent the sample of size 9 from school A, and
# B1 ... B9 are the iid random sample from school B. They carry observed or
# simulated values. 
A1 = rep(0,N) ; B1 = rep(0,N)
A2 = rep(0,N) ; B2 = rep(0,N)
A3 = rep(0,N) ; B3 = rep(0,N)
A4 = rep(0,N) ; B4 = rep(0,N)
A5 = rep(0,N) ; B5 = rep(0,N)
A6 = rep(0,N) ; B6 = rep(0,N)
A7 = rep(0,N) ; B7 = rep(0,N)
A8 = rep(0,N) ; B8 = rep(0,N)
A9 = rep(0,N) ; B9 = rep(0,N)

# The mean of the random variables above, for each N. 
X_bar_A = rep(0,N)
X_bar_B = rep(0, N)

for (i in 1:N) { 
      A1[i] = rnorm(n=1, mean=5, sd=0.25); B1[i] = rnorm(n=1, mean=4.8, sd=0.25)
      A2[i] = rnorm(n=1, mean=5, sd=0.25); B2[i] = rnorm(n=1, mean=4.8, sd=0.25)
      A3[i] = rnorm(n=1, mean=5, sd=0.25); B3[i] = rnorm(n=1, mean=4.8, sd=0.25)
      A4[i] = rnorm(n=1, mean=5, sd=0.25); B4[i] = rnorm(n=1, mean=4.8, sd=0.25)
      A5[i] = rnorm(n=1, mean=5, sd=0.25); B5[i] = rnorm(n=1, mean=4.8, sd=0.25)
      A6[i] = rnorm(n=1, mean=5, sd=0.25); B6[i] = rnorm(n=1, mean=4.8, sd=0.25)
      A7[i] = rnorm(n=1, mean=5, sd=0.25); B7[i] = rnorm(n=1, mean=4.8, sd=0.25)
      A8[i] = rnorm(n=1, mean=5, sd=0.25);  B8[i] = rnorm(n=1, mean=4.8, sd=0.25)
      A9[i] = rnorm(n=1, mean=5, sd=0.25); B9[i] = rnorm(n=1, mean=4.8, sd=0.25)
      
      X_bar_A[i] = mean(c(A1[i],A2[i],A3[i],A4[i],A5[i],A6[i],A7[i],A8[i],A9[i]))
      X_bar_B[i] = mean(c(B1[i],B2[i],B3[i],B4[i],B5[i],B6[i],B7[i],B8[i],B9[i]))
}

hist(X_bar_A, ylab = "Frequency",xlab = "Mean of GPA from School A")
mean(X_bar_A)
sd(X_bar_A)

hist(X_bar_B, ylab = "Frequency",xlab = "Mean of GPA from School B")
mean(X_bar_B)
sd(X_bar_B)




# METHOD 2 ------------------------

# General function: returns a vector of length R where each entry is the
# mean of n data points drawn from the Normal(mu, sigma^2) distribution. 
      # n = sample size to take (number of iid random variables)
      # R = number of replications = number of times we draw the n samples. 
simulate.norm = function(n, R, mu, sigma) {
      x_bar = rep(NA, R) # create empty vector first with length R = num replicates.
      
      for(i in 1:R) {
            # data vector holds the n iid random samples so this would be 
            # equivalent to the data A1, A2, .. A9 above when drawing from
            # the Normal distribution of School A gpa's.
            data = rnorm(n=n, mean=mu, sd=sigma)
            x_bar[i] = mean(data)
      }
      hist(x_bar)
      
      # Comparing the simulated mean and standard deviation with
      # the theoretical ones.
      cat("sim.mean = ", mean(x_bar), "\ttheoretical.mean =", mu,"\n")
      cat("sim.sd = ", sd(x_bar), "\ttheoretical.sd =", sigma/sqrt(n),"\n")
      
      return(invisible(x_bar))
}

X_bar_A_second = simulate.norm(n=9, R=N, mu=5, sigma=0.25)
X_bar_B_second = simulate.norm(n=9, R=N, mu=4.8, sigma=0.25)



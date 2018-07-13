# Question 1

# part c) Probability X-bar is within 0.2 of true mean. 
diff(pnorm(c(-sqrt(2), sqrt(2))))
# [1] 0.8427008


# part d) probability X-bar is within 0.05 of true mean with probability 0.80,
# now we find the sample size n, so find the quantile z
qnorm((1-0.80)/2)
# [1] -1.281552

# Generate random sample of size 1000 from uniform on interval
randUnif <- runif(n=1000, min=-1, max=1)
mean(randUnif)
sd(randUnif)

# theoretical mean: (b + a)/2 = 0
# theoretical sd = (b - a)^2 / 12 = 1/3
sqrt((1+1)^2 / 12)


# b) histogram
hist(randUnif, breaks = seq(from= -1, to=1, by=0.2))
# same thing as if you didn't hand in the breaks. 
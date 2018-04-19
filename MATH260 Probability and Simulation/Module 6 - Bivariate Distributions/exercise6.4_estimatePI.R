
n = 10^5
X = runif(n=n, min=-1, max=1)
Y = runif(n=n, min=-1, max=1)
head(X)

# P(X^2 + Y^2 <= 1) = pi/4
probCircle1 <- sum(X^2 + Y^2 <= 1)/n
pi/4

probCircle1 * 4 # should be about pi = 3.14

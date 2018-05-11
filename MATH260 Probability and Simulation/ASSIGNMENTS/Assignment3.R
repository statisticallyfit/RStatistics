# Question 1

# calculate z-statistic for the 20% of pizzas delivered under 20 minutes:
z0 <- qnorm(0.20, mean=0, sd=1); z0
#[1] -0.8416212 # result

# calculate z-statistic for the 10% of pizzas delivered over 35 minutes:
z1 <- qnorm(0.10, mean=0, sd=1, lower.tail=F); z1
#[1] 1.281552

qnorm(1 - 0.10, mean=0, sd=1) # another way
#[1] 1.281552



# Question 2
beta = 40

# part a) lower quartile: P(T <= t0) = 0.25
qexp(p=0.25, rate=1/beta)
#[1] 11.50728

# part b) P(no goal in a game) = P(T > 90)
pexp(90, rate=1/beta, lower.tail=F)
#[1] 0.1053992

# part c) P(a goal in first 10 min)  = P(T < 10)
pexp(10, rate=1/beta)
#[1] 0.2211992


# part d) P(goal in 20 min if no goal in 10) = P(T < 20 | T > 10)
p1 = diff(pexp(c(10,20), rate=1/beta)) # P(10 < T < 20)
p2 = pexp(10, rate=1/beta, lower.tail=F) # P (T > 10)
p1 / p2
#[1] 0.2211992

# same as just P(T < 10)
pexp(10, rate=1/beta)
#[1] 0.2211992


# part e) P(at least 3 goals in a game) = P(N >= 3) = P(N > 2)
# where N is poisson r.v. with lambda = 9/4
ppois(2, lambda=9/4, lower.tail=F)
#[1] 0.3906607
1 - ppois(2, lambda=9/4) # another way to calculate
#[1] 0.3906607


# part f) P(at least 1 game with no goals in 5 games) = P(B >= 1) = P(B>0)
# where B  is binomial r.v with n = 5, p = P(T > 90)
p = pexp(90, rate=1/beta, lower.tail=FALSE); p
#[1] 0.1053992
# THE FINAL ANSWER IS BELOW:
pbinom(0, size=5, p=p, lower.tail=FALSE)
#[1] 0.4270109
1 - dbinom(0, size=5, p=p) # another way to calculate. 
#[1] 0.4270109



# Question 3

# part a) P(145 <= X <= 163) = P(X <= 163) - P(X <= 144)
diff(pbinom(c(144, 163), size=600, p=0.25))
#[1] 0.5935765

# part b) P(144.5 <= Y <= 163.5)
diff(pnorm(c(144.5, 163.5), mean=600*0.25, sd=sqrt(600*0.25*0.75)))
#[1] 0.5964151

# the approximate value 0.5964151 is close to the exact binomial value 0.5935765



# Question 4

# part b)

# method 2 calculation: 
gamma.density <- function(x){(1/(1000^20 * gamma(20))) * x^(20-1) *exp(-x/1000) }
integrate(gamma.density, lower=30000, upper=Inf)
# 0.02187351 with absolute error < 5.3e-05

# or 
pgamma(30000, shape=20, scale=1000, lower.tail=FALSE)
#[1] 0.02187347

# COMMENT: indeed the first calculation is only correct to 5.3e-5 since
# the two answers differ at the 7th decimal place. The 0.02187347 solution
# is the most exact probability of observing an income in excess of $30,000.
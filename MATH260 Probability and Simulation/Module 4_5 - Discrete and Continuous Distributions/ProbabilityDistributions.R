par(mfrow=c(1,1))

# **************** Normal Distribution **************** 

# d = height of probability density function
dnorm(0, mean=0, sd=1) #same as dnorm(0)
dnorm(0, mean=4, sd=1) #same as dnorm(0, mean=4)
dnorm(0, mean=4, sd=10)
#
v = c(0, 1, 2)
dnorm(v)
#
x = seq(-5, 5, by=0.1)
y = dnorm(x) #the default mean=0, sd=1
plot(x, y)
#
y = dnorm(x, mean=2.5, sd=0.1)
plot(x, y)



# p = cumulative density function
pnorm(0) #same as pnorm(0, mean=0, sd=1)
pnorm(1)
pnorm(0, mean=2, sd=3)
v = c(0, 1, 2)
pnorm(v)
pnorm(-1)
1 - pnorm(1)
pnorm(1, lower.tail=FALSE)
1 - (pnorm(-2) + pnorm(2, lower.tail=FALSE))

normalCdf <- function(lowerBound, upperBound) {
  #same as    1 - (pnorm(-2) + pnorm(2, lower.tail=FALSE))
  return (1 - (pnorm(lowerBound) + 1 - pnorm(upperBound)))
}
normalCdf(-1, 1)
normalCdf(-2, 2)
normalCdf(-3, 3)

x = seq(-5, 5, by=0.1)
y1 = pnorm(x, mean=1, sd=1)
y2 = pnorm(x, mean=2, sd=1)
plot(x, y1, col='red')
plot(x, y2, col='blue')



# q = inverse cumulative density function
qnorm(0.5, mean=0, sd=1) #inverse cdf
qnorm(0.02275013, mean=2, sd=1) # ~ rounds to 0
qnorm(0.5, mean=1, sd=2)
qnorm(0.5, mean=2, sd=2)
qnorm(0.5, mean=2, sd=10)
qnorm(0.25, mean=2, sd=2)
qnorm(0.333)
qnorm(0.333, sd=3)
#
v = c(0.1, 0.3, 0.75) #list of areas
qnorm(v)
#
x = seq(0, 1, by=0.05)
y = qnorm(x, mean=30, sd=2)
plot(x, y, col='blue')



# r = random numbers
rnorm(4)
rnorm(4, mean=3, sd=3)
#
y <- rnorm(200)
hist(y, col='pink')
y <- rnorm(200, mean=-2, sd=4)
hist(y)
#
qqnorm(y)
qqline(y, col='green') #line through q-q plot





# **************** t Distribution **************** 

# dt = height of t distrbution
x <- seq(-20, 20, by=0.5)
y <- dt(x, df=10)
plot(x, y, type='l')
y <- dt(x, df=50)
plot(x, y)



# pt = tcdf
pt(-3, df=10)
1 - pt(3, df=10)
pt(3, df=10)
#
tValue <- (mean(x)-2)/sd(x)
pValue <- pt(tValue, df=20)
pValue



# qt = inverse tcdf
qt(0.05, df=10)
qt(0.10, df=10, lower.tail=FALSE)



# rt = random t-distributed numbers
rt(3, df=10)
rt(10, df=100)






# **************** Binomial Distribution **************** 

# dbinom(x, n, p)
x <- seq(0, 50, by=1)
y <- dbinom(x, 50, 0.2)
plot(x, y)
y <- dbinom(x, 50, 0.6)
plot(x, y)



# pbinom(x, n, p)
pbinom(24, 50, 0.5)
1 - pbinom(25, 50, 0.5)
pbinom(25, 50, 0.5)
pbinom(26, 50, 0.5)
pbinom(25, 51, 0.5)
pbinom(25, 50, 0.25)



# qbinom(area left tail, n, p)
qbinom(0.5, 51, 1/2)
qbinom(0.25, 51, 1/2)
pbinom(23, 51, 1/2)



# rbinom(number of random numbers, n, p)
rbinom(200, 50, 0.2)
rbinom(10, 100, 0.2)
rbinom(5, 100, 0.7)




# **************** Chi-Squared Distribution ***************

# dchisq(x, df=df0)
x <- seq(0, 80, by=0.5)
y <- dchisq(x, df=4)
plot(x, y)
y <- dchisq(x, df=30)
plot(x, y)



# pchisq(x, df=df0)
pchisq(2, df=10) # left tailed
pchisq(2, df=10, lower.tail=FALSE)
pchisq(3, df=20)



# qchisq(left area, df=df0)
qchisq(0.05, df=10)
qchisq(0.05, df=10, lower.tail=FALSE)
qchisq(0.95, df=10)
v <- c(0.005, 0.025, 0.05)
qchisq(v, df=25)



# rchisq(number, df=df0)
rchisq(3, df=10)
rchisq(3, df=70)


# **************** F Distribution ***************
curve(df(x, 6, 11), xlim=c(0, 10), main="F Distribution")
curve(df(x, 19, 5), xlim=c(0, 10), main="F Distribution")
df(2, df1=19, df2=5)

pf(2, df1=19, df2=5)

qf(0.05, df1=19, df2=5)

rf(10, df1=19, df2=5)


# **************** Hypergeometric Distribution ***************

"Suppose you have an urn with 30 balls, 10 red and 20 white. You
select 15 at random. What is the probability that the 
sample contains 8 red? contains 8 or more red?

http://stat.ethz.ch/R-manual/R-patched/library/stats/
html/Hypergeometric.html
http://www.math.grin.edu/~mooret/courses/math335/binom-hyper.html"

dhyper(x=8, m=10, n=20, k=15)
dhyper(x=3, m=7, n=5, k=4)
dhyper(x=3, m=5, n=5, k=3)
dhyper(x=1, m=2, n=8, k=5)
1-dhyper(x=0, m=3, n=9, k=3)
1-phyper(7, 10, 20, 15)
qhyper(0.10, 10, 20, 15)
rhyper(10, 10, 20, 15)

#sample <- rhyper(100, 10, 20, 15)
#hist(sample, breaks=seq(-0.5, 6.5, 1), col='light grey', border='grey')


# **************** Geometric Distribution ***************

"https://stat.ethz.ch/R-manual/R-devel/library/stats/html/
Geometric.html"

x = seq(0, 10, by=1)
y = dgeom(x, 0.2)
plot(x, y)

"
P(X = x) = pq^(x-1)
P(X <= x) = 1-q^x
"

" What is the probability an agent must select 4 people before he
finds one who attended the last game? (p(person attends game) =0.2"
dgeom(4-1, 0.2) # = geometpdf(4, 0.2) = 0.1024

" What is probability he must select more than 6 people before
finding one who attended the game?"
1-pgeom(6-1, 0.2) # = 1-geometcdf(0.2, 6) = 0.262

"Products are made by a machine with a 3% defective rate. What
is probability that first defective occurs in fifth item?"
dgeom(5-1, 0.03)

"What is probability that first defective occurs in first five
inspections?"
# P(X <= 5) = 1- geometcdf(0.03, 5)
pgeom(4, 0.03) #num failures that occur before first success


# par (mfrow = c(2,2))
par (mfrow = c(1,1))
# dp par(mfrow=c(1,1)) to get back normal view
x<-0:4
plot(x+1, dgeom(x, prob = .95),
     xlab = "X = Number of Trials", ylab = "P(X=x)",
     type = "h", main = "First Ready Terminal, p = .95")
x<-0:9
plot(x+1, dgeom(x, prob = .5),
     xlab = "X = Number of Trials", ylab = "P(X=x)",
     type = "h", main = "First Head, p = .5")
x<- 0:19
plot(x+1, dgeom(x, prob = .2),
     xlab = "X = Number of Trials", ylab = "P(X=x)",
     type = "h", main = "First Defective, p = .2")
x<- seq(0, 400, 50)
plot(x+1, dgeom(x, prob = .01),
     xlab = "X = Number of Trials", ylab = "P(X=x)",
     type = "h", main = "First Bit in Error, p = .01")


"Production line has 20% defective rate. What is the min number
of inspections necessary so probability of finding a defective
is more than 75%?"
qgeom(0.75, 0.2) # = after 6 non-defectives, 
#there is at least 75% chance of obtaining the first defective.




# **************** Multinomial Distribution ***************
dmultinom(x=c(3, 2, 1), size=6, prob=c(0.5, 0.3, 0.2)) # x, size, prob
dmultinom(x=c(4, 3, 2, 1), size=10, prob=c(0.54, 0.11, 0.34, 0.01))
#pmultinom() # x, size, prob
rmultinom(n=20, size=6, prob=c(0.5, 0.3, 0.2)) # n, size, prob



# **************** Poisson Distribution ***************
#dpois(x, lambda)
#ppois(q, lambda)
#qpois(p, lambda)
#rpois(n, lambda)
dpois(x=3, lambda=2/5)
ppois(q=3, lambda=3)
1 - ppois(q=2, lambda=3)
1 - ppois(q=4, lambda=3)



# **************** Wilcoxon Distribution ********************
#x <- -1:(4*6+1)
plot(x, dwilcox(x, 4, 6), type="l", lwd=4, col="goldenrod1")
x <- -101:101 # (10*10+1 are the limits)
points(x, dwilcox(x, 10,10), type="l", lwd=4, col="cyan")
#x <- -5:(21*3+1)
points(x, dwilcox(x, 21,3), type="l", lwd=4, col="red")
points(x, dwilcox(x, 2, 3), type="l", lwd=4, col="hotpink")
points(x, dwilcox(x, 3, 21), type="l", lwd=4, col="chartreuse")
x <- -401: 401
points(x, dwilcox(x, 20, 20), type="l", lwd=4, col="purple3")

# basic wilcox graph formula: function(x, n1, n2) 
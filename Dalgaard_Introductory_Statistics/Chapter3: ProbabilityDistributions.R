# random sampling
sample(1:40, 5) # pick 5 numbers from the set 1 to 40
sample(40, 5) # 40 is the length of the vector
sample(40, 50, replace=TRUE)
sample(40, 40, replace=FALSE)

sample(c("H", "T"), 10, replace=TRUE)

sample(c("success", "fail"), 10, replace=TRUE, prob=c(0.9, 0.1))


# combinatorics
prod(5:1)/prod(40:36) # same as 40C5
# way to choose 1 set of particular numbers (5 nums of 40 total)
1/choose(40, 5)
choose(40,5)


# continous distributions (uniform, normal...)
curve(dnorm(x, mean=0, sd=1), xlim=c(-4, 4), main="Normal Density")
curve(dnorm(x), from=-4, to=4)

# discrete distributions (binomial, geometric...)
x <- 0:50
plot(x, dbinom(x, size=50, prob=0.33), type='h')

" situation: 20 guests must decide whether they like treatment 
A or B better. 16 of them like A better. Is this sufficient 
evidence that A is better or could this have happened by chance
if the treatments were equally good?
H0: no difference between treatments: p = 0.5
Ha: treatment A was liked better: p > 0.5 "
pvalue = 1 - pbinom(15, size=20, prob=0.5)
pvalue


# confidence intervals
sigma = 12
n = 5
s = sigma/sqrt(5)
xbar <- 83
# 95% confidence interval
interval = c(xbar + s*qnorm(0.025), xbar + s*qnorm(0.975))
interval


# random numbers
rnorm(10, mean=7, sd=5)
rbinom(10, size=20, prob=1/2)


# --------------------- EXERCISES ------------------------

"3.1
1 - pnorm(3)
1 - pnorm(42, mean=35, sd=6)
dbinom(10, size=10, prob=0.8)
punif(0.9) # this one is obvious...
1 - pchisq(6.5, df=2)
It might be better to use lower.tail=FALSE instead of subtracting from
1 in (a), (b), and (e). Notice that question (c) is about a point probability,
whereas the others involve the cumulative distribution function.

3.2 Evaluate each of the following. Notice that the standard normal can
be used for all questions.
pnorm(-2) * 2
qnorm(1-.01/2)
qnorm(1-.005/2)
qnorm(1-.001/2)
qnorm(.25)
qnorm(.75)

3.3 dbinom(0, size=10, prob=.2)

3.4 Either of the following should work:
rbinom(10, 1, .5)
ifelse(rbinom(10, 1, .5) == 1, H, T)
c(H, T)[1 + rbinom(10, 1, .5)]
The first one gives a 0/1 result, the two others H/T like the sample exam-
ple in the text. One advantage of using rbinom is that its prob argument
can be a vector, so you can have different probabilities of success for each
element of the result."





# 1
1 - pnorm(3)                        #a
1- pnorm(42, mean=35, sd=36)        #b
dbinom(10, size=10, prob=0.8)       #c
punif(0.9)                          #d
1 - pchisq(6.5, df=2)

# 2
area = 0.001 #for area = 1%, 0.5%, 0.1%
c(qnorm(area/2), qnorm(area/2, lower.tail=FALSE))
qnorm(0.25) #quartiles
qnorm(0.75)
pnorm(-2)*2

# 3
dbinom(0, size=10, prob=0.2)
dbinom(10, size=10, prob=0.8)

# 4
rbinom(n=60, size=50, prob=0.8) #binomial is with replacement
# hypergeometric is without replacement, otherwise,they are same
rbinom(n=10, size=1, prob=0.5)              # method 1
ifelse(rbinom(10, 1, .9) == 1, "H", "T")    # method 2
c("H", "T")[1 + rbinom(10, 1, .5)]          # method 3

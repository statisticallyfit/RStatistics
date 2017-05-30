library(ggfortify)


##### Likelihood and loglikelihood ---------------------------------------
lik <- function(p) dbinom(x=630, size=1315, prob = p); lik
loglik <- function(p) dbinom(x=630, size=1315, prob=p, log=TRUE); loglik

# graph them 
# method 1 - likelihood
xs <- seq(0, 1, 0.001)
llData <- data.frame(x=xs, likl=lik(xs), loglikl=loglik(xs))
head(llData)
ggplot(data=llData, aes(x, likl)) + 
      geom_point(shape=18, color="purple") + 
      ggtitle("Binomial Likelihood of Heavy Drinkers") + 
      xlab("Number of students")

# method 1 - loglikelihood 
# help - why does curved shape mean we can use normal dist for confints?
ggplot(data=llData, aes(x, loglikl)) + 
      geom_point(shape=18, color="red") + 
      ggtitle("Binomial Loglikelihood of Heavy Drinkers") + 
      xlab("Number of students")


# method 2 - likelihood
# help why isn't xlabel showing up? 
ggdistribution(dbinom, seq(0, 1315), prob=0.48, size=1315, colour="blue") +
      ggtitle("Likelihood of Heavy Drinkers") + 
      xlab("Number of students")



##### Estimate of MLE ------------------------------------------------------
mle <- optimize(f = lik, interval = c(0, 1), maximum = TRUE); mle
mle$maximum


##### Test for one-sample proportion based on normal approximation ---------
# H0: p 0.5, H1: p is not 0.5
prop.test(x=630, n=1315, p=0.5)

# when n sample size is small we do exact test:
binom.test(x=630, n=1315, p=0.5)



##### Test for one-sample proportion based on Likelihood Ratio Test ----------
# test statistic LR ratio
LR.statistic <- 2 * (loglik(0.48) - loglik(0.5)); LR.statistic
# p-value
p.value <- 1 - pchisq(LR.statistic, df=1); p.value

#### Likelihood ratio confidence 95% interval
cutoff <- loglik(0.48) - 1.92; cutoff # horizontal cutoff

# compute where the cutoff intersects the loglikelihood 
loglik.optim <- function(p) {abs(cutoff - loglik(p))}
min1 <- optimize(f=loglik.optim, interval=c(0, mle$maximum)); min1$minimum
min2 <- optimize(f=loglik.optim, interval=c(mle$maximum, 1)); min2$minimum


# plot cut off and intersection points on the log likelihood
xs <- seq(0.35, 0.65, 0.001)
llData <- data.frame(x=xs, likl=lik(xs), loglikl=loglik(xs))
ggplot(data=llData, aes(x, loglikl)) + 
      geom_point(shape=18, color="red") + 
      geom_hline(yintercept=cutoff) +
      geom_vline(xintercept=min1$minimum) + 
      geom_vline(xintercept=min2$minimum) +
      ggtitle("Binomial Loglikelihood for Heavy Drinkers with cutoff") + 
      xlab("Number of students")

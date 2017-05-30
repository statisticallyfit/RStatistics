data <- read.csv("data/example.csv")
attach(data)

summary(y)

length(table(y)) # shows how many unique y values there are

# a bar per value is uninformative - need to place bin widths well 
plot(range(y), c(0,10), type="n")
for(i in 1:100)
  lines(c(y[i], y[i]), c(0,1), col="blue")


# Calculations using normal z
x <- seq(150, 190, 0.01)
plot(x, dnorm(x, mean=170, sd=8), type="l", lwd=2, col="cyan")


# Graphs of normal distribution
par(mfrow=c(2,2))

x <- seq(150, 190, by=0.01)
pd <- dnorm(x, 170, 8)
# plot 1 and 2 skeletons
plot(x, dnorm(x, 170, 8), type="l")
plot(x, dnorm(x, 170, 8), type="l")
# make plot 1
yv <- pd[x <= 160]
xv <- x[x <= 160]
xv <- c(xv, 160, 150)
yv <- c(yv, yv[1], yv[1])
polygon(xv, yv, col="cyan", border="cyan")
# make plot 2
plot(x, dnorm(x, 170, 8), type="l")
xv <- x[x>= 185]
yv <- pd[x>= 185]
xv <- c(xv, 190, 185) # adding these two to make it more curved
yv <- c(yv, yv[501], yv[501])
polygon(xv, yv, col="maroon1", border="maroon1")
# make plot 3
plot(x, dnorm(x, 170, 8), type="l")
xv <- x[x >= 160 & x <= 180]
yv <- pd[x >= 160 & x <= 180]
xv <- c(xv, 180, 160) # adding these two to make it more curved
yv <- c(yv, pd[1], pd[1])
polygon(xv, yv, col="lightslateblue", border="lightslateblue")


# Quantile plots
data <- read.csv("data/skewdata.csv")
attach(data)
par(mfrow=c(1,1))
qqnorm(values, pch=19, col="deepskyblue")
qqline(values, lty=2, col="navy", lwd=2)


light <- read.csv("data/light.csv")
attach(light)
light
hist(speed)
summary(speed)
boxplot(speed)
qqnorm(speed, pch=19, col="dodgerblue")
# Is Michelson's data different from the believed 299,990 lightspeed?
wilcox.test(speed, mu=990) #since data is not normal


# Bootstrap in hypothesis testing: 

# Question: what is probability that population mean we estimate with
# our random sample of 100 values is as big as 990?
a <- numeric(10000)
for(i in 1:10000)
  a[i] <- mean(sample(speed, replace=TRUE))
hist(a)
boxplot(a)
qqnorm(a)
max(a) # we never got mean equal to 990 in simulation so pvalue~0



# Student t Distribution
 
#show how t value is influenced by degrees of freedom
plot(c(0,30), c(0,10), type="n") # x= df, y= t values
lines(qt(0.975, df=1:30), col="red") #could write 1:30, at start
abline(h=1.96, lty=2, col="green")

# how t looks compared to normal distr
xvs <- seq(-4,4, by=0.01)
plot(xvs, dnorm(xvs), type="l")
lines(xvs, dt(xvs, df=5), col="red")




# Skew: if skew < 0, then left tailed. If > 0, then right tailed
# formula: skew = sum((y - mean(y)^3)/n/(stdev)^3
skew <- function(x) {
  m3 <- sum((x - mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  m3/s3
}

# NOTE: SE (standard error) of the skew = sqrt(6/n)
skew.statistic <- skew(values)/sqrt(6/length(values))
p.value <- 1 - pt(skew.statistic, df=(length(values) - 2))
p.value
# df = n-2 because to find skew we need to know 2 parameters, mean and variance

# Find transformations that normalize the data
# sqrt roots?
skew.statistic <- skew(sqrt(values))/sqrt(6/length(values))
p.value <- 1 - pt(skew.statistic, df=(length(values) - 2))
p.value # still too small
# logs?
skew.statistic <- skew(log(values))/sqrt(6/length(values))
p.value <- 1 - pt(skew.statistic, df=(length(values) - 2))
p.value #yaya! big




# Kurtosis: if > 3, distr is very peaked. If < 3, less peaked than normal distr
# flat topped - platykurtic, peaked - leptokurtic
# formula = sum((y-mean(y)^4)/n/var(y)^2
kurtosis <- function(x){
  m4 <- sum((x - mean(x))^4)/length(x)
  s4 <- var(x)^2
  m4/s4 - 3 #or this way with -3
}

# SE of kurtosis is: sqrt(24/n)

kurtosis.statistic <- kurtosis(values)/sqrt(24/length(values))
1 - pt(kurtosis.statistic, df=(length(values) - 2))
       
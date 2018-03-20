

# Question 1
pmf.W <- function(w) {
      5*w / (6 * (1 + w^2))
}

# a) is valid PDF since it sums to 1
sum(pmf.W(1) + pmf.W(2) + pmf.W(3))
# and all values are positive
all(c(pmf.W(1) > 0, pmf.W(2) > 0, pmf.W(3) > 0))


# b) find E(W)
mu.W <- pmf.W(1) * 1 + pmf.W(2) * 2 + pmf.W(3) * 3
mu.W

# c) var(W)
# E(W^2)
mu2.W <- pmf.W(1) * 1^2 + pmf.W(2) * 2^2 + pmf.W(3) * 3^3
mu2.W
# V(W)
var.w <- mu2.W - mu.W^2 
var.w


# d) TODO why not the same as above? 
var.another.W <- pmf.W(1)*(1-mu.W)^2 + pmf.W(2)*(2-mu.W)^2 + pmf.W(3)*(3-mu.W)^2
var.another.W




# NUMBER 2 Integration
f.X <- function(x) { 2*(1 - x) }
# a)
integrate(f.X, 0, 1) # so is valid prob func
# b)
f.ex <- function(x) {2*x*(1-x)}
EX <- integrate(f.ex, 0, 1)
EX 
names(EX)
EX$value
# c)
f.var <- function(x) { (x - EX$value)^2 * 2 * (1 - x)}
VAR.X <- integrate(f.var, 0, 1)
VAR.X$value




# NUMBER 3 Dice
# a)
diceSample <- sample(1:6, size=120, replace=TRUE, prob=c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6))
table(diceSample)/120
# b) 
mean(diceSample) # true = 3.5
sqrt(var(diceSample)) # true = 1.708
# c)
barplot(table(diceSample))
hist(diceSample)
library(ggplot2)
df <- data.frame(diceSample)
ggplot(df, aes(diceSample)) + geom_histogram(binwidth=1, fill='dodgerblue')

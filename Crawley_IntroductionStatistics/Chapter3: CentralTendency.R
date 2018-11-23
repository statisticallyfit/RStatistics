
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/StatisticsIntroUsingR_Crawley")
getwd()
yvals <- read.csv("data/yvalues.csv")
attach(yvals)
y
hist(y, family="Arial")

# Arithmetic mean
arithmetic.mean <- function(x) sum(x)/length(x)
arithmetic.mean(y)

# median
sorted <- sort(y)
sorted[ceiling(length(y)/2)] # the median
# Or
sort(y)[ceiling(length(y)/2)]

med <- function(x) {
  if(length(x) %%2 == 0)
    (sort(x)[ceiling(length(x)/2)] + sort(x)[ceiling(1+length(x)/2)])/2
  else
    sort(x)[ceiling(length(x)/2)]
}
median(y)
med(y)

# Geometric mean: for processes that change multiplicatively
# formula (product of all y)^(1/n)
insects <- c(1, 10, 1000, 10, 1) #num of insects on 5 plants
exp(mean(log(insects)))

geometric.mean <- function(x) {
  exp(mean(log(x)))
}
geometric.mean(insects)

# NOTE: for populations that fluctuate multiplicatively, 
# graph the log of the population to avoid saying one population
# has bigger variance (it's just that they have different means)
log(100)-log(200) # mean = 150
log(10)-log(20) # mean = 15


# Harmonic mean: used to find average of rates
# formula: n/sum(1/y)

harmonic.mean <- function(x) {
  length(x)/sum(1/x)
}
harmonic.mean(c(1, 2, 4, 1))

detach(yvals)

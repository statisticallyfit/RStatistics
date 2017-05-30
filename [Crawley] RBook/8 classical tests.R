setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstatistics/crawleyRBook")



# Test for normality
x <- exp(rnorm(30))
shapiro.test(x)


cancer <- read.table("data/cancer.txt", header=TRUE)

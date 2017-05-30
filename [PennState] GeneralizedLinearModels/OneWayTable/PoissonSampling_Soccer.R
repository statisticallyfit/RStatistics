setwd("/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/OneWayTable")

library(ggplot2)
library(reshape)

soccer <- read.table("soccer2002.txt", header=TRUE); soccer

# sample size 
n <- sum(soccer$Freq); n 
# sample mean (maximum likelihood estimate (MLE) of lambda)
mu.hat <- (1 / n) * (sum(soccer$Goals * soccer$Freq)); mu.hat

# poisson probability for X = 1 (one goal scored)
dpois(x = 1, lambda = mu.hat)

# poisson probabilities for X = 0,1,...,8 with lambda mu.hat
pi.hat <- dpois(x = 0:8, lambda = mu.hat); pi.hat

# expected frequencies for cell i 
exp.freq <- n * pi.hat; exp.freq

# draw expected vs observed. 
df <- data.frame(freq=soccer$Freq, exp.freq=exp.freq); df

ggplot(data=df, aes(x=soccer$Goals, y=freq)) + 
      geom_point(shape=19, size=3, color="blue") + 
      geom_point(aes(y=exp.freq), color="red", shape=19, size=3) +
      labs(x = "Goals") + 
      ggtitle("Observed and Expected Goal Frequencies")

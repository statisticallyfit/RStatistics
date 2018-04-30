
# jointXY = 1, 0 <= x <= 1, and 0 <= y <= 1
# joint bivariate uniform distribution.

n=10^5
X = runif(n=n, min=0, max=1)
Y = runif(n=n, min=0, max=1)
head(X)

probBothBetweenHalf <- sum((X <= 0.5) & (Y <= 0.5))/n
probBothBetweenHalf



##############################################################
# ANSWER: 

theSim <- 0
# repeat sampling 1000 times
for(i in 1:1000) {
      n = 500 # sample of size 500 from bivariate uniform. 
      u1 <- runif(n=n, min=0, max=1)
      u2 <- runif(n=n, min=0, max=1)
      
      # logical vec depending on whether conditions are met
      probBothBetweenHalf <- sum((u1 < 05.) & (u2 < 0.5)) / n
      
      #store result from each sample (i)
      theSim[i] = probBothBetweenHalf     
}

library(ggplot2)
data <- data.frame(theSim)
ggplot(data, aes(x=theSim)) + geom_density(size=2, colour="hotpink")
ggplot(data, aes(x=theSim)) + geom_histogram(fill="lightpink")

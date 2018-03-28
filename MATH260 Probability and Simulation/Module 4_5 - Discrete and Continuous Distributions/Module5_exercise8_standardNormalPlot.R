
fx <- function(x) dnorm(x, mean=0, sd=1)

curve(expr=fx, from=-4, to=4, ylab="Density of Standard Normal")



##### On same set of axes with distribution
library(ggplot2)

x <- seq(-4, 4, 0.01)
pdf <- dnorm(x, mean=0, sd=1)
cdf <- pnorm(x, mean=0, sd=1)
df <- data.frame(x, pdf, cdf)

ggplot(df, aes(x)) + 
      geom_line(aes(y=pdf), colour="blue") + 
      geom_line(aes(y=cdf), colour="magenta")


#### WITH R BASE FUNCTIONS
plot(x, pdf,  col="blue", type="l", ylim=c(0,1))
lines(x, cdf, type="l", col="red")

setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/SALES.Rdata")
library(ggplot2)

attach(SALES)

# Coding first year sales x and regressing against 2nd year sales (y)
x.i <- SALES$SALESYR1
n <- length(x.i)
x.mean <- mean(x.i)
x.sd <- sqrt(sum( (x.i - x.mean)^2 ) / (n-1) )
u.i <- (x.i - x.mean) / x.sd; u.i
sd(x.i)

sales.lm <- lm(SALESYR2 ~ SALESYR1 + I(SALESYR1^2), data=SALES)
summary(sales.lm)

g1 <- ggplot(SALES, aes(x=SALESYR1, y = SALESYR2)) + 
      geom_point(shape=19, size=3, color="dodgerblue")
g1 #

# correlations
cor(x.i, x.i^2)


#now the intercept coef is significant
sales.coded.lm <- lm(SALESYR2 ~ u.i + I(u.i^2), data=SALES)
summary(sales.coded.lm)

g2 <- ggplot(SALES, aes(x=u.i, y = SALESYR2)) + 
      geom_point(shape=19, size=3, color="dodgerblue")
g2 #

cor(u.i, u.i^2) # much lower than for the xs. 

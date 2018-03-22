setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/EX7_20.Rdata")
library(ggplot2)


EX7_20

# logarithmic decreasing data (with intercept) or exponential decreasing. 
ggplot(data=EX7_20, aes(x=X, y=Y)) + 
      geom_point(shape=19, size=3, color="dodgerblue")

ggplot(data=EX7_20, aes(x= log(X), y= log(Y))) + 
      geom_point(shape=19, size=3, color="dodgerblue") + 
      ggtitle("ln(Y) = B0 + B1 * ln(X)")

log.model <- lm(log(Y) ~ log(X), data=EX7_20)
summary(log.model) # significant, high F-stat and high betas. 

log.model.eq <- function(x){
      log.model$coef[[1]] + log.model$coef[[2]] * x 
}
log.model.eq(30) # ln(y(30)) = B0 + B1 * ln(30)
# Result in y-units original
# e^(ln(y(30)))

# HELP
exp(log.model.eq(30)) # TODO why is this 0????

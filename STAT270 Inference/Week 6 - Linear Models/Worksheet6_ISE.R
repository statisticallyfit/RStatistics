# Exercise 11.8 wackerly

library(ggplot2)
options(show.signif.stars = F)

x.lc50.static <- c(39, 37.5, 22.2, 17.5, 0.64, 0.45, 2.62, 2.36, 32, 0.77)
y.lc50.flow <- c(23, 22.3, 9.4, 9.7, 0.15, 0.28, 0.75, 0.51, 28, 0.39)
data <- data.frame(X = x.lc50.static, Y=y.lc50.flow)

simple.lm <- lm(y.lc50.flow ~ x.lc50.static)
summary(simple.lm)

logx.lm <- lm(y.lc50.flow ~ log(x.lc50.static), data=data)
summary(logx.lm)

ggplot(data, aes(x=x.lc50.static, y=y.lc50.flow)) + geom_point(shape=19)

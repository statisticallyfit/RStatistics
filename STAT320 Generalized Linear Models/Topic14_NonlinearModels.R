setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(MASS) 

options(show.signif.stars = FALSE)

data("wtloss") # from MASS

head(wtloss)


# Exploratory plot
ggplot(wtloss, aes(x=Days, y=Weight))  + geom_point()


# Fit nonlinear model: 2 ^ (-Days / B2)
weight.nls <- nls(Weight ~ b0 + b1 * 2^(-Days / b2), data=wtloss, 
                  start=list(b0=90, b1=95, b2=120))
weight.nls

summary(weight.nls)

# -------------------------------------------------------------------------------------------

# Topic slides: 

# Spawning Data
recruits = c(245, 281, 139, 202, 89, 20, 53, 14)
spawning = c(1980, 1240, 490, 495, 240, 20, 25, 10)
spawnData <- data.frame(recruits, spawning)

ggplot(spawnData, aes(x=spawning, y=recruits)) + geom_point()

# Ricker model for fish data: The usual model for spawning is: y = B0 * x * e^ (-b1 * x)

# HOW TO GET INITIALI START VALUES FOR SPAWN EXAMPLE: 

# B0: for small x, y ~ B0*x, so use the linear line through (0,0), (200,600) to get B0 = 0.3

# B1: for large x, exp(-b1*x) takes over, use the starting value B0 = 0.3 to fit the data point
# (1980, 245) to solve for B1: 
#     y0 = B0 * (x0) (exp(- B1 * x0))
#     b1 = (ln(b0) + ln(x0) - ln(y0)) / x0
#        = (ln(0.3) + ln(1980) - ln(245)) / 1980
#        = 0.0004473

b0 = (200 - 0)/(600 - 0); b0
x0 = 1980
y0 = 245
b1 = (log(b0) + log(x0) - log(y0)) / x0; b1

spawn.nls <- nls(recruits ~ b0 * spawning * exp(-b1 * spawning), 
                 start = list(b0 = 0.3 ,b1 = 0.00044))
summary(spawn.nls)


# So the real curve is: y-hat = 0.498 X exp(-0.000685 x)



# Confidence intervals (95%): 
# Bi_hat +- t_0.975, residualdf=n-2=6 * se(Bi_hat)

cofs <- summary(spawn.nls)$coef
B0  <- cofs[1,1]
se.B0 <- cofs[1,2]
B1 <- cofs[2,1]
se.B1 <- cofs[2,2]
n <- nrow(spawnData)


t.crit <- abs(qt((1-0.95)/2, df = n - 2)); t.crit

B0.ci <- c(B0 - t.crit * se.B0, B0 + t.crit * se.B0); B0.ci 
B1.ci <- c(B1 - t.crit * se.B1, B1 + t.crit * se.B1); B1.ci 

# Or the easy way: 
betaCI(spawn.nls)



# Diagnostics: fitted vs residuals
df.diagnostics <- data.frame(res = residuals(spawn.nls, type="pearson"), fitted = predict(spawn.nls))
# NOTE: the pearson residuals above ARE standardized. Check: 
residuals(spawn.nls, type="pearson")

# Residuals vs fitted. 
ggplot(df.diagnostics, aes(x=fitted, y=res)) + geom_point(size=1) + 
      geom_hline(yintercept=0, linetype="dashed", size=1,color="black") + 
      geom_hline(yintercept=c(-2,2), linetype="dotted", color="black") 
      
# QQnorm
ggplot(df.diagnostics, aes(sample = res)) + 
      stat_qq(color="dodgerblue", size=5, alpha=0.7) + 
      stat_qq_line(linetype="dashed", size=1)

shapiro.test(df.diagnostics$res)
# No non-normality





# Drawing the predicted curve: WITH se confidence bands

# Get covariance matrix (estimated)
cov.hat <- vcov(spawn.nls); cov.hat
xstar <- 1000
# dg/dtheta = [df / d_b0, dg/d_b1]
dg.dtheta <- t(c(xstar * exp(-B1 * xstar), (-xstar)*B0*xstar*exp(-B1*xstar)))
dg.dtheta
# Calculate standard error of y-hat (s_y-hat) at xstar = 1000
var.at1000 <- dg.dtheta %*% cov.hat %*% t(dg.dtheta)
var.at1000
se.at1000 <- sqrt(var.at1000)
se.at1000

# Can calculate the 95% ci for the Ricker curve at x = 1000 using the standard error
yhat <- B0 * xstar * exp(-B1*xstar); yhat

# Average Y Interval: 
yhat + c(-1,1) * abs(qt((1-0.95)/2, df=n-2)) * se.at1000
# ===> we are 95% confident that E(y | x = 1000) is between 213 and 289 million recruits. 
 #(average number, not individual prediction. )

# Prediction Y Interval: (for individual new observation)
# Var(y | x* = 1000) = s^2 + s_yhat^2
# yhat +- t(0.975, df = n-2) * sqrt(s^2 + s_yhat^2)
s <- summary(spawn.nls)$sigma; s
yhat + c(-1,1) * qt(0.975, df=n-2) * sqrt(s^2 + se.at1000^2)
# ===> we are 95% confident that a future new observation with 1000 thosuand tonnes
# of spawning stock would produce between 175 and 327 million recruits. 


newSpawn.df <- data.frame(spawning = seq(from=0,to=2000, by = 0.1)) # len= nrow(spawnData)
pred <- predict(spawn.nls, newdata=newSpawn.df)
pred.df <- data.frame(spawning = newSpawn.df$spawning, pred)

ggplot(spawnData, aes(x=spawning, y=recruits)) +  geom_point(size=1, shape=19) +
      geom_line(data=pred.df, aes(y=pred, colour="red"),size=1) 
      #geom_line(data=df, aes(y=ProbLower),colour="dodgerblue", linetype="dashed",size=1) + 
      #geom_line(data=df, aes(y=ProbUpper), colour="dodgerblue", linetype="dashed",size=1) 
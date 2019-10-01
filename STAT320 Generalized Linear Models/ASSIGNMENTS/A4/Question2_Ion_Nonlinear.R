setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/ASSIGNMENTS/A4/plotTools.R')


library(ggfortify)


options(show.signif.stars = FALSE)

# part a) -------------------------------------------------------------------------------

log10x <- c(-9, -7, -5.96, -4.97, -4, -3.08)
x = 10^log10x
y <- c(8.56, 8.94, 13.82, 32.16, 56.68, 85.46)

ionData <- data.frame(leadConc = x, logLeadConc = log10x, ElectroForce = y)

ggplot(ionData, aes(x=leadConc, y=ElectroForce)) + geom_point(size=3)

ggplot(ionData, aes(x=logLeadConc, y=ElectroForce)) + geom_point(size=3, colour="dodgerblue")



# Find initial values: 
# c_inint: 
c_init <- 10 ^ (-6.5) # determines where the curvilinear region begins
# b_init: using slope of linear portion of the curve to find b_init: 
# (-5.96, 13.82), (-3.08, 85.46) ==> slope is: b_init = (85.46 - 13.82) / (-3.08 + 5.96) = 
b_init = (85.46 - 13.82) / (-3.08 + 5.96) ; b_init

# a_init: use the b, c, and point (10^-9, 8.56)
# a_init = y - b*log(x + c)
y0 = 8.56
x0 = 10^(-9)
a_init = y0 - b_init * log10(x0 + c_init); a_init


# Fit the data using the nonlinear model: y = a + b * log(lead + c) 
ion.nls <- nls(ElectroForce ~ a + b*log10(leadConc + c), 
               start=list(a=a_init, b = b_init, c = c_init), data= ionData)
summary(ion.nls)


# part b) ---------------------------------------------------------------------------------

# Graph showing original data points and fitted curve (x = log10))
logLeadConc.values <- seq(from = min(ionData$logLeadConc), to = max(ionData$logLeadConc), by = 0.011)
leadConc.values <- 10^logLeadConc.values

# Above need to get the non-log values since we fitted the model with leadConc not log of leadConc
# so we need to pass this into the predict() function
ion.preds <- predict(ion.nls, list(leadConc = leadConc.values))
pred.df <- data.frame(logLeadConc = logLeadConc.values, pred = ion.preds)

ggplot(data=ionData, aes(x=logLeadConc, y=ElectroForce)) + geom_point(size = 4) + 
      geom_line(data=pred.df, aes(y = pred), color="dodgerblue", size=2)

# Perfect fit

# part c) ) ---------------------------------------------------------------------------------

# Calculate 95% confidence interval for slope (b)
betaCI(ion.nls)
# b between (27.0122, 32.36)
# Given b_ideal = 29.2)
# INTERPRET: the ideal value is in the 95% CI so our model estimates seem to
# coincide with the ideal value gained from universal laws. 

#-----

# Manually: 
#df.residual <- summary(ion.nls)$df[2]
#df.residual
s <- summary(ion.nls)$sigma # called the standard error of the regr model
s # residual standard error
#SSE <- s^2 * df.residual
#SSE

SSxx <- sum( (ionData$leadConc - mean(ionData$leadConc))^2 )
SSxx
s.b0 <- s / sqrt(SSxx); s.b0


            
# part d ) ---------------------------------------------------------------------------------

# Manual prediction: 
leadPPM <- 30 # 30 parts per million
leadMolPerLiter <- 30 * 4.8 * 10^(-6); leadMolPerLiter

# Line:  y = 175.8242 + 29.53907 * log10(leadConc + 0.000002170153)

yhat.line <- function(leadConc) {
      a.hat <- coef(ion.nls)[[1]]
      b.hat <- coef(ion.nls)[[2]]
      c.hat <- coef(ion.nls)[[3]]
      
      return (a.hat + b.hat * log10(leadConc + c.hat) )
}

      
yhat.line(leadMolPerLiter)


# Predict() function: 
predict(ion.nls, list(leadConc = leadMolPerLiter))

# ==> expected ise measurement for an average soil sample is 62.537 mV. 


# part e) ---------------------------------------------------------------------------------

# Prediction interval for when leadConc = leadMolPerLiter
a.hat <- coef(ion.nls)[[1]]
b.hat <- coef(ion.nls)[[2]]
c.hat <- coef(ion.nls)[[3]]

# dg/dtheta = [df / da, df / db,             df/dc]
#           = [1      , log10(leadConc + c), b/( ln(10) * (leadConc + c)) ]
dg.dtheta <- t(c(1, log10(xstar + c.hat), b.hat / (log(10) * (xstar + c.hat))))

calcPredictionCI.nls(leadMolPerLiter, ion.nls, dg.dtheta, yhat.line)

calcPredictionCI.nls <- function(xstar, fit.nls, dg.dtheta, yhat.line){
      
      cov.hat <- vcov(fit.nls)
      
      # Calculate standard error of y-hat (s_y-hat) at xstar 
      var.atXstar <- dg.dtheta %*% cov.hat %*% t(dg.dtheta)
      
      yhat <- yhat.line(xstar)
      
      df.residual <- summary(fit.nls)$df[2]
      s <- summary(fit.nls)$sigma # residual standard error
      predInterval <- suppressWarnings(yhat + c(-1,1) * abs(qt(0.975, df=df.residual)) * sqrt(s^2 + var.atXstar))
      
      return(predInterval)
}

calcPredictionCI.ionFit <- function(xstar){
      a.hat <- coef(ion.nls)[[1]]
      b.hat <- coef(ion.nls)[[2]]
      c.hat <- coef(ion.nls)[[3]]
      
      cov.hat <- vcov(ion.nls)
      
      # dg/dtheta = [df / da, df / db,             df/dc]
      #           = [1      , log10(leadConc + c), b/( ln(10) * (leadConc + c)) ]
      dg.dtheta <- t(c(1, log10(xstar + c.hat), b.hat / (log(10) * (xstar + c.hat))))
      
      # Calculate standard error of y-hat (s_y-hat) at xstar 
      var.atXstar <- dg.dtheta %*% cov.hat %*% t(dg.dtheta)
      
      yhat <- yhat.line(xstar)
      
      n <- nrow(ionData)
      s <- summary(ion.nls)$sigma # residual standard error
      predInterval <- suppressWarnings(yhat + c(-1,1) * abs(qt(0.975, df=n - 2)) * sqrt(s^2 + var.atXstar))
      
      return(predInterval)
}

calcPredictionCI.ionFit(leadMolPerLiter)

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

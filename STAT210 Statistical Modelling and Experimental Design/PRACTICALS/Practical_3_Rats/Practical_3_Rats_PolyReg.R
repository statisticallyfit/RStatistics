setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_2_Perch/')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/INTERPRET.R')

library(ggplot2)
options(digits = 10, show.signif.stars = FALSE)

# xs = concentrations in ml / L of vaccine
conc <- c(rep(seq(from = 0.5, to = 3, by = 0.5), 2))
conc #
# ys = skin response in rats
skin <- c(13.9, 14.08, 13.75, 13.32, 13.45, 13.59, 13.81, 13.99, 13.60, 13.39,
          13.53, 13.64)
ratsData <- data.frame(Skin=skin, Conc=conc)

# Plotting data
plot(skin ~ conc) # seems negative x^3 model with B3 < 0

ggplot(ratsData, aes(x = Conc, y = Skin)) + 
      geom_point(shape=19, size=3, color="dodgerblue")



# To determine the order of polynomial that best fits data: 
# Step 1: fit linear + quadratic terms + test significance of quadratic
# given linear term is fitted. Continue if singificant. 
# Step 2: continue fitting terms until non-sig result is obtained
# Step 3 - use residuals and qqplot to examine model that includes terms
# up to the last significant term to see if that model gives good fit to data.
# Step 4 - if fit is good, accept current model. If not, repeat steps 2,3,4 until
# good fit is found. 

skin2.lm <- lm(Skin ~ Conc + I(Conc^2), data=ratsData)
summary(skin2.lm)
anova(skin2.lm)

skin3.lm <- lm(Skin ~ Conc + I(Conc^2) + I(Conc^3), data=ratsData)
summary(skin3.lm)
betaCI(skin3.lm)
anova(skin3.lm) # seeing that after fitting the linear and quadratic term,
# the cubic term is still significant. 

skin4.lm <- lm(Skin ~ Conc + I(Conc^2) + I(Conc^3) + I(Conc^4), data=ratsData)
summary(skin4.lm)
betaCI(skin4.lm)
anova(skin4.lm)

# if n = sample, can only fit polynomial up to degree n-1, so here the limit
# is a fifth degree poly. 
# Singularities ... can't do this
skin6.lm <- lm(Skin ~ Conc + I(Conc^2) + I(Conc^3) + I(Conc^4)
               + I(Conc^5) + I(Conc^6), data=ratsData)
summary(skin6.lm)
anova(skin6.lm)



# FITTING FIFTH DEGREE (continuing on in steps)
skin5.lm <- update(skin4.lm, .~. + I(Conc^5), data=ratsData)
summary(skin5.lm)
anova(skin5.lm)
# So now ANOVA says we get fifth term isn't significant, so we backtrack to skin4

# Testing residuals variance (plot 1) + qq plot (plot 2)
par(mfrow=c(1,2))
plot(skin4.lm, which=1:2)

library(ggfortify)
autoplot(skin4.lm, which=1:2)

# plot 1 = red line suggests curvature but that is mainly for observation # 3
# good scatter, cenetered resids around yhat = 0 line
# plot 2 = relatively normal

shapiro.test(skin4.lm$residuals) # no reason to reject null that 
# resids are normal. 


# Plot 95% CI and PIs

xs <- data.frame(Conc=seq(0.5, 3, 0.05))
head(xs)
# the se.fit is the stderrors of the fitted values for each fitted value
# in the sequence 0.5 to 3 by 0.05 intervals (to be more fine-grained for plotting)
#pred <- predict(skin4.lm, newdata=xs, se.fit=TRUE, interval="confidence")
pred <- data.frame(predict(skin4.lm, newdata=xs, interval="confidence"))
pred$Conc <- xs$Conc

# plot response, predictor
par(mfrow=c(1,1))
plot(Skin ~ Conc, ylim = c(min(pred$lwr), max(pred$upr)), data=ratsData)
# add fitted line to plot
lines(xs$Conc, pred$fit, lty=1)
lines(xs$Conc, pred$lwr, lty=2)
lines(xs$Conc, pred$upr, lty=2)

legend(x=2,y=14.2, legend=c("Fitted Curve", "95% Confidence Bands"), lty=1:2)


# Using ggplot


p = ggplot(ratsData, aes(x=Conc, y=Skin)) + 
      geom_point(shape=19, size=3) 

p2 = p + geom_line(data=pred, aes(y=fit, colour="a", linetype="a"),size=1) +
    geom_line(data=pred, aes(y=lwr, colour="b", linetype="b"),size=1) + 
    geom_line(data=pred, aes(y=upr, colour="b", linetype="b"),size=1) 

p2 + scale_colour_manual(name="Legend", values=c("a"="red", "b"="dodgerblue"),
                         labels=c("Fitted Line", "95%\nConfidence\nBands")) +
  scale_linetype_manual(name="Legend", values=c("a"="solid", "b"="dashed"),
                        labels=c("Fitted Line", "95%\nConfidence\nBands"))

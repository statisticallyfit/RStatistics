setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 6, 7 - Variable Screening, Transformations/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R")


options(digits=10, show.signif.stars = FALSE)

data("LifeCycleSavings") # data on savints ratio from 1960-1970
# sr = numeric = aggregate personal savings  (RESPONSE)
# pop15 = numeric = % population under 15
# pop75 = numeric = %population over 75
# dpi = numeric = real per-capita disposable income
# ddpi = numeric = % growth rate of dpi. 

#pairsQuantPlot(LifeCycleSavings, c(2:5, 1))

pairs(LifeCycleSavings[, c(2:5,1)], lower.panel = panel.smooth, 
      upper.panel = panel.cor)

# INTERPRET: 
# strong negative cor between pop15 and pop75 (r = -0.91)
# influential point between ddpi and sr. 
# # ddpi is not strong cor between any of the others. 
# Need only subset of pop15, pop75, and dpi since they are strongly correlated.
# Need only pop15 since cor is highest among the three (pop15, pop75, dpi), 
# and just ddpi since its cor is lowest. 
start.model <- lm(sr ~ 1, data=LifeCycleSavings)
full.model <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=LifeCycleSavings)

step(start.model, direction = "forward", scope=formula(full.model))

step(full.model, direction = "backward", scope=formula(start.model))


# Model 
sr.lm <- lm(sr ~ pop15 + ddpi, data=LifeCycleSavings)
summary(sr.lm)

autoplot(sr.lm)
shapiro.test(sr.lm$residuals)
# residuals - random scatter, centered around 0. 
# good straight normal line. Just one outlier Zambia since beyond or near 3. 
# (R by default identifies the 3 most extreme values, but doesn't mean they
# are problematic. )

# Residuals vs. Leverage: 
par(mfrow=c(2,2))
plot(sr.lm, add.smooth = F, cook.levels = c(0.2, 0.5, 1.0))
par(mfrow=c(1,1))
plot(sr.lm, add.smooth = F, cook.levels = c(0.2, 0.5, 1.0), which=5)




# mean leverage h.mean = 2(k+1)/n, n = 50, k = 2
k = 2; n = 50
h.mean = 2*(k + 1)/n; h.mean
# onplot, Libya has leverage = 0.45 > 0.12 so it is influential. 


# influential values: 
influentialPoints(sr.lm)
cooksDistance(sr.lm)


# more measures: https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html





# Predictions
preds <- predict(sr.lm)
preds#
sr <- LifeCycleSavings$sr #
pop15 <- LifeCycleSavings$pop15
ddpi <- LifeCycleSavings$ddpi
resids <- sr.lm$residuals
cbind(sr, preds, resids, pop15, ddpi)

# INTERPRET: Zambia has largest residual = 10.48
# Chile has resid -7.58, and Japan is 7.71. 
# LIBYA is influential (because on the plot) and calculation.  Has far off
# values of pop15 and ddpi. 
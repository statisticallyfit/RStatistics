setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/DIESEL.Rdata")
attach(DIESEL)

# Fitting categorical model, no interaction
diesel.lm <- lm(PERFORM ~ FUEL + BRAND, data=DIESEL)
summary(diesel.lm)

# Fitting with interactions
diesel.interact.lm <- lm(PERFORM ~ FUEL + BRAND + FUEL:BRAND, data=DIESEL)
summary(diesel.interact.lm)

interactionPlot(data=DIESEL, xFactor = "FUEL", traceFactor = "BRAND",
                response="PERFORM")
with(DIESEL, interaction.plot(FUEL, BRAND, PERFORM))

# Estimating mean engine performance when F3 and B2

# FOR MAIN EFFECTS MODEL
# F3 means x2 = 1 so x1 = 0 and B2 means x3 = 1
# So E(y) = B0 + B2 + B3
B0 <- diesel.lm$coefficients[[1]]; B0
B2 <- diesel.lm$coefficients[[3]]; B2
B3 <- diesel.lm$coefficients[[4]]; B3
mu_32 <- B0 + B2 + B3; mu_32



# INTERACTION MODEL
# F3 means x2 = 1, x1= 0, and B2 means x3 = 1
# So E(y) = mu_32 = B0 + B2 + B3 + B5
B0 <- diesel.interact.lm$coef[[1]]; B0
B2 <- diesel.interact.lm$coef[[3]]; B2
B3 <- diesel.interact.lm$coef[[4]]; B3
B5 <- diesel.interact.lm$coef[[6]]; B5
mu_32 <- B0 + B2 + B3 + B5; mu_32 #



# Mean CI's and prediction CIs
# HELP NOT WORKING
#predict(diesel.lm, newdata = data.frame(conc=seq(0,1,1)))
#meanCI(diesel.lm, x.values=c(0, 1, 1))

#predict(diesel.lm, newdata = data.frame(rbind(c(0,1,1))))
#meanCI(diesel.lm, x.values=c(0, 1, 1))


# Test for validity of interaction terms
anova(diesel.lm, diesel.interact.lm)

NestedFandChiSqTest(diesel.lm, diesel.interact.lm)

# Fstat is significant so there is evidence that Fuel type and Brand do interact.
# Should keep them in the model (choose the full model)

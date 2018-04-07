setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 5 - Principles of Model Building/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')

library(ggplot2)


powerData <- read.table("POWERLOADS.txt", header=TRUE)
#attach(powerData)
#detach(powerData)
head(powerData)

# exploratory plot
ggplot(powerData, aes(x=TEMP, y=LOAD)) + 
      geom_point(shape=19, size=3, color="dodgerblue") + 
      ggtitle("Plot of Temperature vs. Power Load")

# Fitting first order model
power1.lm <- lm(LOAD ~ TEMP, data=powerData)
summary(power1.lm)
# INTERPRET: Temp is very significant linear predictor for power Load. 

residualFittedPlot(power1.lm) # seems to be a curvature/pattern, there is not
# much random scatter so we are missing a term in the model, not constant, not
# around zero. 
partialPlot(power1.lm, variableName = "TEMP")

normalQQPlot(power1.lm)

shapiro.test(power1.lm$residuals)



# Fitting quadratic
# note: if you don't use I for temp^2 you get interaction mdoel
power2.lm <- lm(LOAD ~ TEMP + I(TEMP^2), data=powerData)
summary(power2.lm)
# significant curvature term and linear term

library(ggfortify)
residualFittedPlot(power2.lm)
normalQQPlot(power2.lm)


# residual plot shows more random scatter - better
anova(power2.lm)

# ANOVA TABLE vs COEFFICIENTS TABLE: 
# *** Anova table tests each term in order of fit
# *** the coefficints table tests each term in the model conditioning (allowing
# for the fact that) the other terms have been fitted ALONGSIDE the one we are 
# testing. 
# THIS IS WHY THE LAST TERM P_VALUES IN EACH ROW FOR EACH TABLE ARE THE SAME. 

# hierarchical: if interaction is significant, must keep the main effects
# even if they aren't significant. 

# Hierarchical: need to keep lower order terms if the p-term is significant. 
# But any p-value of preceding p  terms are not useful interpretable. 





# FIT THE CUBIC
power3.lm <- lm(LOAD ~ TEMP + I(TEMP^2) + I(TEMP^3), data=powerData)
summary(power3.lm)
anova(power3.lm)

autoplot(power3.lm, which=1:2)
# ANOVA LINE -- TEMP: is the linear term significant? YES
# ANOVA line -- TEMP^2: is the quadratic term sig. after we fit linear term? YES
# ANOVA LINE -- TEMP^3: is the cubic term sig. after we fit linear+quadratic? NO
# --> so we don't ened the linear term. 

# COEFFICIENTS TABLE: these are the p-values of the coefficients when we are
# fitting the linear, quadratic and cubic terms together. NOT MEANINGFUL
# to look at p-values lower than the highest p-term (p=3 cubic here) because
# they are testing whether we need the lower order terms once we've fitted
# the p = 3 term. YES OF COURSE because the model is hierarchical, doesn't
# make sense without the lower order terms < p if you have highest term p. 








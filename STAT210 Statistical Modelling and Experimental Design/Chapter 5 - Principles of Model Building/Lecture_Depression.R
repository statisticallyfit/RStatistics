setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 5 - Principles of Model Building/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')

library(ggplot2)
library(lattice)


depressionData <- read.table("depression.txt", header=TRUE)

# exploratory plot


# response - measure of treatment effectiveness. 
xyp <- xyplot(response ~ age | treat, data=depressionData, 
              layout=c(3,1), # by cols = 3, then rows = 1
              panel=function(x,y) { 
                    panel.xyplot(x,y);  # scatterplot
                    panel.lmline(x, y) }) # least squares on top of each scatter
xyp

# INTERPRET: as age increases, effectiveness increases across all treats A,B,C 
# overall. For treat A and B, effectiveness increases less than for treat C. 
# Not much difference between treats A and B. 
# Increase in effectivenss with age is greatest for treat C. 
# For younger ages (20) treat A works more effectively than for treats B, C
# For older people, A and C seem more efffective than treat B. 




# MODEL: 

# Y_ij ~ N(mu_ij, sigma^2)

# mu_ij = B0 + B1 * age_ij + B2*treatB + B3*treatC + B4 * age_ij : treatB + 
# B5 * age_ij : treatC

depress.A.lm <- lm(response ~ age * treat, data=depressionData, x=TRUE)
depress.A.lm$x
anova(depress.A.lm)
summary(depress.A.lm)
# INTERPRET: 
# the interaction term age:treat is significant so we can start with interaction mdoel
# Meaning: not all slopes are the same. Slope for treatC is maybe significantly different
# from A, B. (at least one slope is different from the other)



betaCI(depress.A.lm)
# CONCLUDE: treats A and C means are different (last row) but treats A and B
# means are not (2nd last) since the confint contains a zero. 
# The rate of change of effectiveness with increase in age is different for
# treatments A and C, but not between A and B. 
# 2nd last - 0 difference in slopes is a reasonable value. But for confint A,C
# 0 is not a reasonable difference so slope for C is significantly greater
# than for A since confint is above is zero. 


attach(depressionData)
#tapply(response, treat, mean) # can't use this here since age is QUANTITATIVE. 



# FITTING MODEL WITH NO INTERCEPT (individual means) when many levels
depress0.lm <- lm(response ~ treat/age - 1, data=depressionData)
anova(depress0.lm)

betaCI(depress0.lm)





# NEW BASELINE = B
depressionData$treat <- relevel(depressionData$treat, ref="B")
depress.B.lm <- update(depress.A.lm)
betaCI(depress.B.lm)

# INTERPRET: C and B are significantly different, since confint for treatC
# doesn't contain zero, and mean intercept level for C is sig. lower than for B. 
# 2. mean intercept level for A is significantly greater than for B (treatA confint)
# 3. slope difference is not significant for A and B (age:treatA confint)
# 4. slope difference is significant for C and B and greater for C (age:treatC confint)




residualFittedPlot(depress.B.lm) # little increase in variance of error in the middle
# but doesn't look too extreme. 

normalQQPlot(depress.B.lm)
shapiro.test(depress.B.lm$residuals)





# Sorting/ confidence bands
depressionData$treat <- relevel(depressionData$treat, ref="A")
ord <- order(depressionData$treat, depressionData$age)
ord
depressionData.ord <- depressionData[ord, ]
depressionData.ord

preds <- predict(depress.A.lm, newdata=depressionData.ord, interval="confidence")
predsAndData <- cbind(depressionData.ord, preds)

fit.plot <- xyplot(response + fit + lwr + upr ~ age|treat,
                   data=predsAndData, layout=c(3,1),
                   ylab="response", panel=plot.profiles)
fit.plot

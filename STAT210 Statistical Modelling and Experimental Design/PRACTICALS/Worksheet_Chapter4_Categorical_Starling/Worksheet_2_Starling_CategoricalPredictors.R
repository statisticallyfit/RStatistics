setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 4 - Multiple Linear Regression")


library(ggplot2)
options(digits=10, show.signif.stars = FALSE)



starlingData <- read.table("starling.txt", header=TRUE)
starlingData$Roost <- as.factor(starlingData$Roost)
head(starlingData)




# Means don't seem significantly diffrent, just between Roost 4 and 1, 
# and gradually lowering. 
# Between roost 1 and 2 and roost 1 and 3 doesn't seem significantly different. 
# Question of constant variance since variability in roost 1 is greater
# Outlier in roost 4 so should come up in residuals. 
# Distributions for 1 and 4 are skewed (positively)
ggplot(starlingData, aes(x=Roost, y = Mass, group=Roost, color=Roost)) + 
      geom_boxplot(outlier.colour = "black", outlier.size=4, lwd = 1) + 
      ggtitle("Effect of Roost on Starling Mass")


# Linea rmodel - base roost is 1
starling1.lm <- lm(Mass ~ Roost, data=starlingData)
summary(starling1.lm)
# Base mean-hat mass is Roost 1 (intercept) = 83.60 mass at roost 1 (REJECT H0)
# mean-hat mass at Roost 2 = roost2coef  + intercept = 83.6 - 4.20 = 79.4 (REJECT H0)
# mean-hat mass at roost 3 = roost3coef + intercept = 83.6 - 5 = 78.6 (REJECT H0)
# mean-hat mass at roost 4 = roost4coef + intercept = 83.60 - 8.20 = 75.40 (REJECT H0)

# Global F is significant so there is some difference in mean masses at each
# roost. 


# residuals plots
library(ggfortify)
# straight line is typical of four types of fitted vals for the 4 levels. 
autoplot(starling1.lm, which=1:2)
# constant variance - residuals seem centered evenly around yhat=0 line
# normal qq plot: seem to hug the line except outliers observations 36 and 22
shapiro.test(starling1.lm$residuals)


# Reset base level to Roost 3
starlingData3 <- starlingData
starlingData3$Roost <- relevel(starlingData$Roost, ref="3")
starling3.lm <- lm(Mass ~ Roost, data=starlingData3)
summary(starling3.lm)

# before: 
# intercept: MEANroost1
# B1 = MEANroost2 - MEANroost1
# B2 = MEANroost3 - MEANroost1
# B3 = Meanroost4 - Meanroost1

# Now: (with base 3)
# intercept: MEAN_HAT_roost3
# B1 = MEAN_HAT_roost1 - MEANroost3
# B2 = MEANroost2 - MEANroost3
# B3 = Meanroost4 - Meanroost3

# So extra information is: difference in means of roosts 2 and 4 with 3
# NO DIFF BETWEEN ROOST 2 and 3
# THERE IS SIG DIFF BETWEEN ROOST 3 and 4

# And: before had difference of roost 3 and 1 but now same but inversed sign since
# : roost1 - roost3


starling.nointercept.lm <- lm(Mass ~ Roost - 1, data=starlingData)

betaCI(starling.nointercept.lm)
# If not overlap between confints then we can be certain there is a significant
# difference between the means. But if there is a lot of overlap, then
# not much significant difference. 
# PREFER HYP TEST SINCE CONFINTS ARE MORE FUZZY. 
# GREAT DIFF BETWEEN 1 and 4 ROOSTs but not for 1,2,3

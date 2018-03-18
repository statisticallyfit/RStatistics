setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ASSIGNMENTS/A1/')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')
#source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
#source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/INTERPRET.R')

options(digits = 3, show.signif.stars = FALSE)

hollyData <- read.table("Hollywood.txt", header=TRUE)
head(hollyData)


# part a) plotting with pairs()
pairs(hollyData[, c(2,3,4,1)], lower.panel = panel.smooth, upper.panel = panel.cor)

# Summary: 


### Production-Promo
# There is a moderate positive linear correlation of 0.79 between
# Promo and Production; it is less strong since 0.79 is farther
# from 1; and it is  positive since 0.79 > 0. The scatter plot shows this, 
# as a few points are farther from
# the smoothing line compared to the points in the Promo-Receipts plot. 

### Production - Books
# There is a weak positive linear correlation of 0.43 between Production
# and Books. This is shown by the the scatter - there are at least 2 or 3
# peaks in the scatter of Books when Production is on the x-axis. Correlation
# only picks up linear relationships, and not polynomial or curved ones. 

### Books - Promo
# When Books is on the y-axis and Promo on the x-axis, there is once again
# what seems to be a cubic relation in the scatter. 
# The r = 0.30 > 0 so it is a positive linear correlation but it is very weak
# since 0.30 is closer to 0 than to 1. 


## -----

### Receipts - Production
# There is a strong positive linear correlation of 0.92 between Receipts
# and Production, as shown by the scatter of the first graph on the bottom row. 
# The scatter travels upward and seems close to the smoothing line, 
# which indicates the strong linear relation. 
# The correlation r is positive since r = 0.92 > 0 and strong since
# it is near 1.

### Receipts - Promo
# There is an even stronger positive linear correlation between Receipts and
# Promo since r = 0.93. This is near 1, so the linear relation is strong, and
# positive, so as Receipts increase, Promo increases. This is shown by the
# scatter in the second graph of the bottom row - the data points are 
# very close to the upward smoothing line. 

### Receipts - Books
# But there seems to be a curvilinear relation between Books and Receipts. 
# As Books increases, Receipts undergoes several peaks. The overall linear
# correlation r = 0.47, which is slightly weak, and positive. 


# part b) 
receipts.lm <- lm(Receipts ~ Production + Promo + Books, data=hollyData)
# table of regression coefficients
print(summary(receipts.lm))
# Anova table
print(anova(receipts.lm))



# part c) Global F-test
# H0: B1 = B2 = B3 = 0
# Ha: at least one population B coefficient is not zero. 
# The F-statistic for the model = 58.2 and p-value = 0.0000791 < alpha=0.05
# So we can reject the null hypothesis at 5% significant level. This means
# The model is overall statistically useful for predicting Receipts (y). 


# part d)
# H0: B1 = 0
# Ha: B1 != 0 (not zero)
# the t-statistic of B1 (production) is t = 3.28 and p-value = 0.0169 < alpha=0.05
# so we can reject the null hypothesis at 5% significance level to conclude
# there is enough evidence that the production is an important predictor
# of Receipts. Rejection of the null means there is a significant linear relation
# between Receipts and production. 

# H0: B2 = 0
# Ha: B2 != 0
# The t-statistic is t = 4.60 and p-value = 0.0037 < alpha=0.05 so we can reject
# the null hypothesis at 5% significance level. We conclude there is a significant
# linear relation between Promo and Receipts; Promo is an important predictor
# of receipts. 

# H0: B3 = 0
# Ha: B3 != 0
# The t-statistic is t = 1.54 and p-value = 0.1754 > alpha = 0.05, so we
# fail to reject the null hypothesis. We conclude there is not enough evidence
# to say there is a significant linear relation between Books and Receipts. 



# part e)
# Overall, the model is statistically useful for predicting Receipts, as
# suggested by the global F-test in c). But in part d), the estimated B3
# coefficient for Books is not significant, so it seems Books is not an 
# important predictor of Receipts. 
# Therefore we should only include Production and Promo as predictors. 



# part f)
receipts2.lm <- lm(Receipts ~ Production + Promo, data=hollyData)
print(summary(receipts2.lm))
print(anova(receipts2.lm))


# part g)
betaCI(receipts2.lm)

interpret.SlopeCI(receipts2.lm)

# INTERPRETATIONS: 
# B1: With 95% confidence, when Promo is held fixed, then for each 1 million
# dollar increase in Production, the population mean number of Receipts per millions
# increases between 1.5018 and 6.9547 receipts per millions. 

# B2: With 95% confidence, when Production is held fixed, then for each 1 million
# dollar increase in Promo, the population mean number of Receipts per millions
# increases between 3.1648 and 11.7074 receipts per millions. 
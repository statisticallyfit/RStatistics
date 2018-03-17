setwd('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_2_Perch/')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/INTERPRET.R')

library(GGally)
options(digits = 3, show.signif.stars = FALSE)

# read data
perchData <- read.table("Perch.txt", header=TRUE)
head(perchData)

# Pairs plot
# cols 3,4,2 = cols Length, Width, Weight
pairs(perchData[, c(3, 4, 2)], lower.panel = panel.smooth, lwd=3, col="blue", upper.panel = panel.cor)

# Ssee other options for continuous X, categorical Y etc data: ...
# https://cran.r-project.org/web/packages/GGally/GGally.pdf
ggpairs(data=perchData, columns=c(3,4,2), 
        lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="density", params=c(colour="magenta")),
        upper = list(continuous="cor", params=c(size=10)))

# INTERPRETATION: 
# 1. strong positive linear correlations between  predictors (length, width). 
# 2. strong pos linear correlations between predictors (length, width) with response
# is 0.96 cor of length with weight, and 0.964 cor of width with weight. 
# 3. relationship between predictors (width and length) is linear
# but relation between (length, weight (y)) and (width, weight(y)) are both 
# curvilinear, in the positive upward direction. 



# Main effects model with length width as preds
perch.maineffects.lm <- lm(Weight ~ Length + Width, data=perchData)
summary(perch.maineffects.lm)
# INTERPRET: 
# 1. t-tests for individual model parameters are singificant, indicating
# that predictors are important for predicting perch weight (y). 
# 2. Adj R^2: About 93.5% of sample variation in perch weight can be 
# explained by this linear model with length and width as predictors. 

# residuals vs fitted plot
par(mfrow=c(1,1))
plot(perch.maineffects.lm, which=1)
# INTERPRET: normality of residuals is not satisfied - there is a definite
# curve/pattern for the residuals. Curved nature suggests missing term
# in the model. The pairs plot suggests a curvilinear relation between y (weight)
# and each of the predictors but WE think an interaction term can explain this. 
# Longer fish may have greater increase in weight for each cm increase in width
# compared with shorter fish. 
autoplot(perch.maineffects.lm)


# part h) i) ii)
perch.interact.lm <- lm(Weight ~ Length + Width + Length:Width, data=perchData)
summary(perch.interact.lm)
# Interpret: the interaction term is highly significant, but intercept
# is just below significance: p-value = 0.058 > 0.05, and predictor
# of length is not singificant: p-value = 0.274. 
# SIGNIFICANT INTERACTION means change in weight as length increases
# is not the same for all widths. (plot has non-parallel lines for different
# width values). 

# Adj R^2 = About 98.4% of sample variation in weights is explained by the model
# using the interaction term. 

# residuals vs fitted
plot(perch.interact.lm, which=1)
# Residuals seem more centered along the y-hat = 0 line, except for a little
# off for higher fitted values. So variance of errors seems more constant. 
# But three observations 52, 50, and 55 seem to be outliers. 
# NOTE: observation fish # 52 is unusually heavy for its length/width
# and observation fish # 55 is unusually width. 
autoplot(perch.interact.lm, which=1)
autoplot(perch.interact.lm)
# These are the ones outside -2*s, 2*s bounds, where s = 44.2, 
# so the range is (-88.4, 88.4), and also we
# can see they are outside the -2, 2 range on the QQ plot. 
autoplot(perch.interact.lm, which=2)
plot(perch.interact.lm, which=2)


# Testing Normality of errors
autoplot(perch.interact.lm, which=2)
shapiro.test(perch.interact.lm$residuals) # null: resids are normal
# and we have reason to reject null and conclude resids aren't normal. 


# part i)
# OVERALL: inclusion of interaction term is justified but we still have
# doubts about the validity of the model assumptions. 


# part j)
summary(perch.interact.lm)$coef


# part k) MeanCI with length=30 cm and width=6cm
lengthVal <- 30; widthVal <- 6;

meanCI(perch.interact.lm, x.values=c(lengthVal, widthVal))
# We are 95% confident that the population mean weight of perch with length = 30 cm
# and width = 6 cm will lie between 314 and 429 gms. 


# TODO FIX
interpret.MeanCI(perch.interact.lm, x.values=c(lengthVal, widthVal),
                 x.units=c("cm", "cm"), y.unit="pounds")

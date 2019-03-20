setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/")
options(show.signif.stars = FALSE)

library(ggplot2)
library(ggfortify)

bikeData <- read.csv("bike.csv", header=TRUE)


# part a) histogram of casual users (number of casual users per day, the response variable)

# note: breaks = 15 implies 15+1 = 16 classes
numClasses = 16
# formula for calculating binwidth is: (max - min) / number of classes = class width
# Calculating: 
theBinWidth = round(diff(range(bikeData$Casual)) / numClasses); theBinWidth

ggplot(data=bikeData, aes(x=Casual)) + 
      geom_histogram(color="white", fill="dodgerblue",binwidth=theBinWidth)

# Or can do it with regular R
hist(bikeData$Casual, breaks=15)

# Distribution is highly right-skewed with long right tail (skewness would be > 0)
# This means over the 200 days, there are more likely to be few casual bike users
# than many casual bike users. This is shown by the fact that most of the distribution
# is gathered on the left, where the response has low value, and there are few
# high values of casual users, shown by the long right tail. 

# part b) --------------------------------------------------

pairs(bikeData[,1:8]) # include all predictors but the last, since Casual = response

# There seems to be no correlation between variables since either the plots contain
# a cloud of dots, with no linear pattern, or they are vertical series or horizontal
# series of dots, again indicating no linear correlation. 


# part d) ------------------------------------------------------------------------
bikeData$Weather <- factor(bikeData$Weather)
bikeData$Year <- factor(bikeData$Year)


# part c) --------------------------------------------------
ggplot(data=bikeData, aes(x=Weather, y=Casual, colour=Weather)) + 
      geom_boxplot(size=1) 

# There are more casual bike users for mild weather (1 = clear, few clouds) than there
# are for heavy thunderstorms (3) and mist (2), indicated by the greather height in
# the boxplot at 1 than at 2 and 3. Also there is greater variation in casual bike users
# for mild weather (1), signified by the larger IQR at 1 than for the other weathers. 

# part e) ----------------------------------------------------------
bike.lm <- lm(Casual ~ .- Date, data=bikeData)
summary(bike.lm)

# significant predictors: SeasonSpring, Year2012, Weekend, Temperature, Humidity, Windspeed
# SeasonSpring - significantly more casual users in spring than autumn (base level)
# Year2012 = significantly more casual users in 2012 than 2011. 

# part f) ----------------------------------------------------------

head(bikeData)

# Date, Year, Season, Weekend, Weather are categorical
# Use polynomial fits only for: Temperature, Humidity, Windspeed

# TEMPERATURE MODEL ------ 
# Using the backward approach: 

# temp1.lm <- lm(Casual ~ Temperature, data=bikeData)
# summary(temp1.lm) # continue since fit is significant
# temp2.lm <- lm(Casual ~ Temperature + I(Temperature^2), data=bikeData)
# summary(temp2.lm) #  quadratic coefficient is not significant

temp5.lm <- lm(Casual ~ poly(Temperature, 5), data=bikeData)
summary(temp5.lm) # no coefficient is significant
temp4.lm <- lm(Casual ~ poly(Temperature, 4), data=bikeData)
summary(temp4.lm) # no coefficient is significant, so keep reducing until find
# a significant coefficient
temp3.lm <- lm(Casual ~ poly(Temperature, 3), data=bikeData)
summary(temp3.lm) # all coefficients are significant, especially the third order, keep.

# checking diagnostics for the best higher order model: order 3
autoplot(temp3.lm, which=c(1,2))
# CONCLUSION: not a good fit. Data is not normal, there are outliers, and residuals
# have a funnel shape pattern, indicating increasing variance in residuals. 


# WINDSPEED MODEL ------

wind1.lm <- lm(Casual ~ Windspeed, data=bikeData)
summary(wind1.lm) # marginally significant windpseed coefficient (just above significance)
wind2.lm <- update(wind1.lm, . ~ . + I(Windspeed^2), data=bikeData)
summary(wind2.lm) # no coefficient is significant
wind3.lm <- update(wind2.lm, . ~ . + I(Windspeed^3), data=bikeData)
summary(wind3.lm) # no coefficient is significant
wind4.lm <- update(wind3.lm, . ~ . + I(Windspeed^4), data=bikeData)
summary(wind4.lm) # no coefficient is significant
wind5.lm <- update(wind4.lm, . ~ . + I(Windspeed^5), data=bikeData)
summary(wind5.lm) # no coefficient is significant

autoplot(wind1.lm, which=c(1,2))
# CONCLUSION: funnel pattern in residuals indicates increasing variance of residuals, 
# and qq plot shows non-normality at the extreme tails of the data. 
# NOte: there is less strong funnel shape for wind1 than for other higher order
# wind models
autoplot(wind5.lm, which=c(1,2))


# HUMIDITY MODEL ------
# Using the backward approach: 
humid1.lm <- lm(Casual ~ Humidity, data=bikeData)
summary(humid1.lm) # significant coefficient, so continue
humid2.lm <- update(humid1.lm, . ~ .  + I(Humidity ^ 2), data=bikeData)
summary(humid2.lm) # no more significant coefficients
humid3.lm <- update(humid2.lm, . ~ .  + I(Humidity ^ 3), data=bikeData)
summary(humid3.lm) # significant 3rd order term, so continue
humid4.lm <- update(humid3.lm, . ~ .  + I(Humidity ^ 4), data=bikeData)
summary(humid4.lm) # significant 4th order term, so continue
humid5.lm <- update(humid4.lm, . ~ .  + I(Humidity ^ 5), data=bikeData)
summary(humid5.lm) # no more significant coefficients, so use model 4

autoplot(humid4.lm, which=c(1,2))
# CONCLUSION: strong funnel shape in residuals, strong non-normality, not good model fit



# part g) ---------------------------------------------------------------------------

# Fitting multiple regression with the higher order terms from f)
# humid4, temp3, wind1

head(bikeData)
bike.multiple.lm <- lm(Casual ~ Season + Year + Weekend + Windspeed + 
                             poly(Humidity, 3) + poly(Temperature, 3),data=bikeData)

summary(bike.multiple.lm3)

autoplot(bike.lm)
shapiro.test(bike.lm$residuals)
# funnel shape in residuals, curvature - missing predictor?
# residuals not normal
# many outliers and leverage points
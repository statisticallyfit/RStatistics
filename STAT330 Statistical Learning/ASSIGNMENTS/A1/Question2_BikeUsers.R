setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT330 Statistical Learning/ASSIGNMENTS/A1/")
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
      geom_histogram(color="white", fill="dodgerblue",binwidth=theBinWidth) + 
      ggtitle("Counts of Casual Bike Users Per Day") + 
      xlab("Number of Casual Users") + ylab("Frequency")

# Or can do it with regular R
# hist(bikeData$Casual, breaks=15)

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

# Part c) below part d) so I could make some variables factors beforehand 

# part d) ------------------------------------------------------------------------
bikeData$Weather <- factor(bikeData$Weather)
bikeData$Year <- factor(bikeData$Year)
bikeData$Weekend <- factor(bikeData$Weekend)

# part c) --------------------------------------------------
ggplot(data=bikeData, aes(x=Weather, y=Casual, colour=Weather)) + 
      geom_boxplot(size=1)  + 
      ggtitle("Number of Casual Bike Users Across Weather Conditions")

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

# Date, Year, Season, Weekend, Weather are categorical
# Use polynomial fits only for: Temperature, Humidity, Windspeed

# TEMPERATURE MODEL ------ 

# Using the backward approach: 
temp5.lm <- lm(Casual ~ Temperature+I(Temperature^2) + I(Temperature^3) +
                     I(Temperature^4) + I(Temperature^5), data=bikeData)
summary(temp5.lm) # no coefficient is significant
temp4.lm <- update(temp5.lm, .~. -I(Temperature^5), data=bikeData)
summary(temp4.lm) # no coefficient is significant, so keep reducing until find
# a significant coefficient
temp3.lm <- update(temp4.lm, .~. -I(Temperature^4), data=bikeData)
summary(temp3.lm) # 3rd order coeff is significant. 
temp2.lm <- update(temp3.lm, .~. -I(Temperature^3), data=bikeData)
summary(temp2.lm) # go back to 3rd order since no more coefficients are significant.

# checking diagnostics for the best higher order model: order 3
autoplot(temp3.lm, which=c(1,2))
# CONCLUSION: not a good fit. Data is not normal, there are outliers, and residuals
# have a funnel shape pattern, indicating increasing variance in residuals. 


# WINDSPEED MODEL ------
wind5.lm <- lm(Casual ~ Windspeed + I(Windspeed^2) + I(Windspeed^3) + 
                I(Windspeed^4) + I(Windspeed^5), data=bikeData)
summary(wind5.lm)# no coefficient is significant
wind4.lm <- update(wind5.lm, .~. -I(Windspeed^5), data=bikeData)
summary(wind4.lm) # no coefficient is significant
wind3.lm <- update(wind4.lm, .~. -I(Windspeed^4), data=bikeData)
summary(wind3.lm) # no coefficient is significant
wind2.lm <- update(wind3.lm, . ~ . - I(Windspeed^3), data=bikeData)
summary(wind2.lm) # no higher order coefficient is significant
wind1.lm <- lm(Casual ~ Windspeed, data=bikeData)
summary(wind1.lm) # marginally significant windpseed coefficient (just above significance)

# Using the backward approach, start with the 5th order model and continue until
# find a model with a significant coefficient. This happens at order 1 (linear)
autoplot(wind1.lm, which=c(1,2))

# CONCLUSION: funnel pattern in residuals indicates increasing variance of residuals, 
# and qq plot shows non-normality at the extreme tails of the data. 
# NOte: there is less strong funnel shape for wind1 than for other higher order
# wind models
autoplot(wind5.lm, which=c(1,2))


# HUMIDITY MODEL ------
# Using the backward approach: 
humid5.lm <- lm(Casual ~ Humidity + I(Humidity^2) + I(Humidity^3) + I(Humidity^4) + 
                      I(Humidity^5), data=bikeData)
summary(humid5.lm) # no  significant coefficients, reduce further
humid4.lm <- update(humid5.lm, .~. -I(Humidity^5), data=bikeData)
summary(humid4.lm) # significant 4th order term, so continue
humid3.lm <- update(humid4.lm, .~. -I(Humidity^4), data=bikeData)
summary(humid3.lm) # significant 3th order term, so continue
humid2.lm <- update(humid3.lm, .~. -I(Humidity^3), data=bikeData)
summary(humid2.lm) #  no  significant coefficients, so keep the previous 3rd order

# Using the forward approach, we can keep the 4th order model. 
# Using the backward approach, we start with the 5th order model and see the last
# significant model is the 3rd order one, so we keep the third order model. 
autoplot(humid3.lm, which=c(1,2))
# CONCLUSION: strong funnel shape in residuals, strong non-normality, not good model fit



# part g) ---------------------------------------------------------------------------

# Fitting multiple regression with the higher order terms from f)
# humid3, temp3, wind1

bike.multiple.lm <- lm(Casual ~ Season + Year + Weekend + Windspeed + 
                             Humidity + I(Humidity^2) + I(Humidity^3) + 
                             Temperature + I(Temperature^2) + I(Temperature^3), data=bikeData)

summary(bike.multiple.lm)

autoplot(bike.multiple.lm, which=c(1,2))
shapiro.test(bike.multiple.lm$residuals)
# funnel shape in residuals, curvature - missing predictor?
# residuals not normal
# many outliers and leverage points

# part h) ---------------------------------------------------------------------------
summary(bike.multiple.lm)

# (i)
# Weekend1 coefficient is negative (significantly): negative means there is higher
# number of casual bike use on weekend (0=base level) than on weekdays. 
# note: 0 = weekend, 1 = weekday

# (ii)
# As windspeed increases by 1 km/h, the number of casual bike users is expected
# to decrease by about 19 people (or 20), holding other predictors constant. 
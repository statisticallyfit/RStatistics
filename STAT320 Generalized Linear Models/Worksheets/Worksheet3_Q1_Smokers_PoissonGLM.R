setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')

library(ggplot2)
library(ggfortify)
options(show.signif.stars = FALSE)


# Question 1: Example 6.2 (in topic 6)

# OFFSET: 
# The data are counts and modelled with a Poisson distribution. 
# The number of person-years
# are not the same for all age groups and this has to be incorporated into the model.
# If we view the response as deaths/ person-years, then whilst the numerator has a
# random component, the denominator does not. We need to adjust the systematic part 
# to account for the denominator but not the random part.

# RULE WHEN TO USE OFFSET in GLM: when the area , or time period is ot the same
# for all response couts, then we need to use an offset. 

#### NNOTE on offset: 
# log(Y / area) = XB 
#### is the same  as
# log(Y) = XB + log(area)
#### So we have that the offset is the log(area)



# Preparing the data
age <- 1:5
numDeaths <- c(32, 104, 206, 186, 102)
personYears <- c(52407, 43248, 28612, 12663, 5317)

smokerData <- data.frame(Age=age, NumDeaths=numDeaths, PersonYears=personYears)
smokerData


# Part a) --------------------------------------------------------------------------
ggplot(data=smokerData, aes(x=age, y=NumDeaths)) + 
     geom_point(shape=19, size=3, color="blue") 
     ggtitle("Deaths")

# plot the number of deaths / 100,000 person yeras as function of age. 
ggplot(data=smokerData, aes(x=age, y=NumDeaths/(PersonYears/100000))) + 
     geom_point(shape=19, size=3, color="blue") + 
     #geom_smooth(method="lm", color="red") +  # the predictions 
     ggtitle("Deaths per 100,000 Person Years")

# or the school plot: 
plot(numDeaths * (100000/personYears) ~ age)


# Part b, c) --------------------------------------------------------------------------
# Fit glm with poisson error

smoker.offset.glm <- glm(NumDeaths ~ age + offset(log(PersonYears)), data=smokerData,
                         family=poisson)

anova(smoker.offset.glm, test="Chisq")

# Residual Deviance Test (comparing successes vs observed)
ResidualDevianceTest(smoker.offset.glm) # so model is not a good fit
ResidualDevianceTest.Poisson(smoker.offset.glm)
# DevianceTest(smoker.offset.glm) # note this doesn't work since the null model
# that I fit inside doesn't take into account offset. 

# Use this instead to calculate deviance manually..

# Deviance Test = (global F-test for glm)
dev = smoker.offset.glm$null.deviance - smoker.offset.glm$deviance; dev
df <- smoker.offset.glm$df.null - smoker.offset.glm$df.residual; df
pValue <- 1 - pchisq(dev, df); pValue # p < 0.05 so model is statistically useful
# since at least one of the coefficients is nonzero (global F-test for glm)


# Part d,e,f) -----------------------------------------------------------------------

# Fitting a quadratic term: 

smoker.offset.quad.glm <- glm(NumDeaths ~ age + I(age^2) + offset(log(PersonYears)),
                              data=smokerData, family=poisson)
# Is the model a good fit? 
ResidualDevianceTest(smoker.offset.quad.glm, printNice=F) # yes, p-value > 0.05

# Is model good for prediction? (global F-test)
DevianceTest(smoker.offset.quad.glm) #, printNice=F) # yes, since p-value = 0

# Base R way:
nullmodel <- glm(NumDeaths ~ offset(log(PersonYears)), data=smokerData, family=poisson)
anova(nullmodel, smoker.offset.quad.glm, test='Chisq')
anova(nullmodel, smoker.offset.glm, test="Chisq")
anova(smoker.offset.quad.glm, test="Chisq")

# Parameters
summary(smoker.offset.quad.glm)


# Compare the tables of the two models with ANOVA:
anova(smoker.offset.glm, test="Chisq")
anova(smoker.offset.quad.glm, test="Chisq")

#CONCLUSION: 
# This quadratic model is much better since both the residual deviance and
# the deviance tests pass. 


# Part g) --------------------------------------------------------------------------

## INTERPRETING: exponents for Poisson glm: 

### If β = 0, then exp(β) = 1, and the expected count, μ = E(y) = exp(α), 
# and Y and X are not related.

### If β > 0, then exp(β) > 1, and the expected count 
# μ = E(y) is exp(β) times larger than when X = 0
# -> "increase X_i by 1 unit multiplies the expected Y (count) by exp(B_i)"
# -> "for a 1 unit increase in X_i, the expected Y (count) increases 
# by 100*(exp(B_i)-1) percent. "

### If β < 0, then exp(β) < 1, and the expected count 
# μ = E(y) is exp(β) times smaller than when X = 0
# -> "increasing X_i by 1 unit multiplies the expected Y(count) by exp(B_i)"
# -> "for a 1 unit increase in X_i, the expected Y(count) decreases by 
# 100*(exp(B_i) - 1) percent. "


c <- coef(smoker.offset.quad.glm) ; c
### AGE: for a 1 year increase in age, the log(number of deaths) increases by 
# about 2.0257
# Since the coefficient = 2.02 > 0, then the response number of deaths will
# increase (on the multiplicative order)

exp(c)
# EXACT: for a 1 year increase in age, the NUMBER OF DEATHS increases and will be 
# multiplied by exp(2.02) = 7.538 for each age increase.

100*(exp(c)-1)
# BEST: for a 1 year increase in age, the NUMBER OF DEATHS rate is 100(exp(2.02)-1) = 
# = 658.19% higher. Or a subject older by one year will have a rate of number of
# deaths that is 658% higher. 



## INTERPRETING: confidence intervals ---------------------------------------------
b <- betaCI(smoker.offset.quad.glm); b
ci <- confint(smoker.offset.quad.glm); ci

## AGE: 
exp(ci)
# -> we are 95% confident that increasing he age by 1 year multiplies the expected
# number of deaths by between e^(1.6, 2.41) = (5.22, 11.17).
100*(exp(b)-1)
# -> we are 95% confident that for a 1 year increase in age, the expected number
# of deaths increases by between 100*(e^(1.6, 2.41) - 1) = (422.777, 1017) percent.
# So there is between 422% and 1017% increase in number of deaths for a 1 year
# increase in age. 


# Part h) --------------------------------------------------------------------------
## CALCULATING the fitted values on the response scale. 

preds <- predict(smoker.offset.quad.glm, type="response")
preds


# Part i) --------------------------------------------------------------------------

# Verify prediction for age = 1
### log(deaths) = XB + log(personYears)
# OR
### deaths = exp(XB) * personYears

numDeathsRegLine <- function(index){
     
     cs = coef(smoker.offset.quad.glm); cs
     predNumDeaths <- exp(cs[1] + cs[2]*age[index] + cs[3]*age[index]^2) * personYears[index]
     
     return(predNumDeaths[[1]])
}
numDeathsRegLine(1)



# Plotting the deaths vs age and predicted values
plot(numDeaths * (100000 / personYears) ~ age, ylab="Deaths / 100,000 person years", 
     xlab="age")
points(age, preds*100000/personYears, pch=2, colour="red")
legend(4, 500, legend=c("observed", "predicted"), pch=1:2)

# plotting ggplot
# modelPlot(smoker.offset.quad.glm)
plotConfidenceBands.glm(smoker.offset.quad.glm) # TODO: does this do as supposed to?
# OR

scatter <- ggplot(data=smokerData, aes(x=Age, y=NumDeaths/(PersonYears/100000))) + 
     geom_point(shape=19, size=3) 
scatter

df <- data.frame(ageSeq=seq(min(age), max(age), by=0.01))
predictions <- scatter + 
     geom_point(data=smokerData, aes(colour="red", y=preds/(PersonYears/100000)))
predictions

# SOURCES: 
# ON HOW TO INTERPRET POISSON COEFFICIENTS: 
# (1) https://newonlinecourses.science.psu.edu/stat504/node/168/
# https://stats.idre.ucla.edu/stata/output/poisson-regression/
# (2) http://environmentalcomputing.net/interpreting-coefficients-in-glms/
# (3) http://webcache.googleusercontent.com/search?q=cache:http://biostat.mc.vanderbilt.edu/wiki/pub/Main/CourseBios312/poisson.pdf
# (4) formula derivation: http://www.haowang.pw/blog/Poisson-Coefficient-Interpretation/
# (5) exact: https://stats.stackexchange.com/a/128942
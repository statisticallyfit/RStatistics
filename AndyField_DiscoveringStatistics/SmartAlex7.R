library(ggplot2)
library(dplyr)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstatistics/DiscoveringStatswithR")

# Task 1 ------------------------------------------------------------

# Create regression model
pubsData <- read.delim("data/pubs.dat", header=TRUE)
head(pubsData)

ggplot(data=pubsData, aes(x=pubs, y=mortality)) + 
      geom_point(shape=19) + 
      geom_smooth(method="lm")

pubsModel <- lm(data=pubsData, mortality ~ pubs)
pubsModel
# Significant F shows number of pubs predicts mortality well.
summary.lm(pubsModel)

# Bootstrap coefficients
bootReg <- function(formula, data, indices){
      d = data[indices, ] # rows with sampled indices, all cols
      fit = lm(formula, data=d)
      return(coef(fit))
}
bootRegResults <- boot(data=pubsData, statistic=bootReg, 
                       formula=mortality~pubs,
                       R=2000)
res <- bootRegResults$t
head(res) # first col = intercept values, second col = slope values

# Intercept confint
boot.ci(bootRegResults, type="bca", index = 1)
# Slope confint
boot.ci(bootRegResults, type = "bca", index = 2)

# The confints don't cross 0, so the number of pubs still 
# significantly predicts mortality


# Task 2 ------------------------------------------------------------
library(QuantPsyc) # lm.beta
library(car) # dwt

supermodel <- read.delim("data/Supermodel.dat", header = TRUE)
head(supermodel)

supermodelModel <- lm(data = supermodel, salary~age+years+beauty)
supermodelModel
summary.lm(supermodelModel)
summary.aov(supermodelModel)
lm.beta(supermodelModel)

# Is the model valid?
dwt(supermodelModel) # x values are not autocorrelated
vif(supermodelModel) # no higher than 10
1/vif(supermodelModel) # no lower than 0.1

# Casewise diagnostics
supermodel$cooks <- cooks.distance(supermodelModel)
supermodel$residuals <- resid(supermodelModel)
supermodel$stand.resids <- rstandard(supermodelModel)
supermodel$stud.resids <- rstudent(supermodelModel)
supermodel$dfbeta <- dfbeta(supermodelModel)
supermodel$dffit <- dffits(supermodelModel)
supermodel$leverage <- hatvalues(supermodelModel)
supermodel$cov.ratios <- covratio(supermodelModel)

head(supermodel)

# list of standardized residuals > 2 or < -2
supermodel$large.stand.resids <- abs(supermodel$stand.resids) > 2
mean(supermodel$large.stand.resids) # 5.1% of cases
# Which cases were the residuals large?
supermodel[supermodel$large.stand.resids, c(1:4, 7)]

# PLOTS

# Rstandard frequency plot
ggplot(supermodel, aes(stand.resids)) + geom_histogram(fill="dodgerblue")
# fitted vs standardized residuals
d1 <- data.frame(fits = supermodelModel$fitted.values, 
                rstand = supermodel$stand.resids)
ggplot(d1, aes(x=fits, y=rstand)) + geom_point(shape=19)
# qqplot for fitted
qplot(sample=supermodelModel$fitted.values, stat="qq")
# fitted vs residuals
d2 <- data.frame(fits = supermodelModel$fitted.values, 
                resids = supermodel$residuals)
ggplot(d2, aes(fits, resids)) + geom_point(shape=19) 



# Task 4 -----------------------------------------------------------
child <- read.table("data/ChildAggression.dat", header=TRUE)
head(child)

# Parenting: high score is bad parenting
# Computer: high scores is more computer, as with TV
#: Diet: high score is good diet
# SiblingAggression: high score is more aggression in older sibling
# Aggression: (in younger child) (y-value, rest are predictors)

aggModel1 <- lm(data=child, Aggression ~ Sibling_Aggression + 
                        Parenting_Style)
aggModel2 <- lm(data=child, Aggression ~ Sibling_Aggression + 
                      Parenting_Style + Diet + Computer_Games + 
                      Television)
summary.lm(aggModel1)
summary.lm(aggModel2)

anova(aggModel1, aggModel2) # model2 explains more y-variance 

lm.beta(aggModel1) # Parenting more important
lm.beta(aggModel2) # Diet, computer, parenting, siblingaggression

vif(aggModel1) # less than 10, no multicollinearity
1/vif(aggModel1)
vif(aggModel2)
1/vif(aggModel2)

dwt(aggModel1) # yes, independent errors
dwt(aggModel2)

# PLOTS

# Frequency rstandard plot
d1 <- data.frame(x=rstandard(aggModel2))
ggplot(data=d1, aes(x)) + geom_histogram()

d2 <- data.frame(x=rstandard(aggModel1))
ggplot(d2, aes(x)) + geom_histogram()

# fitted versus rstandard
# Homoskedasticity (no funneling) 
# as well as independence of errors (random pattern)
d3 <- data.frame(x=aggModel1$fitted.values, y=rstandard(aggModel1))
ggplot(d3, aes(x,y)) + geom_point()

d4 <- data.frame(x=aggModel2$fitted.values, y=rstandard(aggModel2))
ggplot(d4, aes(x,y)) + geom_point()

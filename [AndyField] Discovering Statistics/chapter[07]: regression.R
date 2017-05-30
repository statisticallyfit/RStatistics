library(ggplot2)
library(boot)
library(car)
library(QuantPsyc)

#install.packages("QuantPsyc")

getwd()
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstatistics/DiscoveringStatswithR")

# ------ Example 1 ------
album1 = read.delim("data/Album Sales 1.dat", header=TRUE)
head(album1)

album1Model = lm(sales ~ adverts, data=album1)
summary(album1Model)
summary.lm(album1Model) # same thing
summary.aov(album1Model)
r = cor(album1$sales, album1$adverts); r

# ------ Example 2 ------
pubsData = read.delim("data/pubs.dat", header=TRUE)
pubsData

pubsModel = lm(mortality ~ pubs, data=pubsData)
summary.lm(pubsModel)
summary.aov(pubsModel)

ggplot(data=pubsData, aes(x=pubs, y=mortality)) + 
  geom_point(shape=19) + 
  geom_smooth(method="lm", lwd=1)

# ------ Example 3 ------
dfbetaData = read.table("data/dfbeta.dat", header=TRUE)
head(dfbetaData)

# data with influential point
dfbetaModel = lm(Y ~ X, data=dfbetaData)

ggplot(data=dfbetaData, aes(x=X, y=Y)) +
  geom_point(shape=19) + 
  geom_smooth(method="lm", lwd=1)

summary.lm(dfbetaModel)
summary.aov(dfbetaModel)

adj.pred.value = 29 - 0.90*1; adj.pred.value
adj.pred.parameter = -0.90
adj.pred.intercept = 29

# data without influential point
dfbetaDataClean = dfbetaData[(dfbetaData$CASE != 30),]

dfbetaModelClean = lm(Y ~ X, data=dfbetaDataClean)

ggplot(data=dfbetaDataClean, aes(x=X, y=Y)) + 
  geom_point(shape=19) + 
  geom_smooth(method="lm", lwd=1)

summary.lm(dfbetaModelClean)
summary.aov(dfbetaModelClean)

original.pred.value = 31 - 1*(1); original.pred.value
original.pred.parameter = -1
original.pred.intercept = 31

# influence statistics 
DFFit = adj.pred.value - original.pred.value; DFFit

DFBeta.slope = adj.pred.parameter - original.pred.parameter; DFBeta.slope
DFBeta.intercept = adj.pred.intercept - original.pred.intercept; DFBeta.intercept



# Cross-validation: assessing accuracy of model for different samples
# METHODS: 
# 1. Adjusted R^2: variance in Y that would be explained if the model 
#     had been derived from the population
# 2. Data splitting: split data randomly into halves and make new 
#     regression models and compare their R^2 and b-values

album2 = read.delim("data/Album Sales 2.dat", header=TRUE)
head(album2)

album2Model = lm(sales ~ adverts, data=album2)
album2Model 
summary.lm(album2Model)
album3Model = lm(sales ~ adverts + airplay + attract, data=album2)
album3Model
summary.lm(album3Model)
# OR
album3Model = update(album2Model, .~. + airplay + attract)
# Cross-validity of this model is good since adj.Rsquared is 
# close to Rsquared

# lm.beta standardizes parameters, reducing them to stdev units
# and making them easier to compare amongst each other
lm.beta(album3Model) # looks like airplay is most important
confint(album3Model) #interval for attract is wider (worse)


# Comparing Fit of the Models (comparing R^2)
# formula: F = (N - k - 1) R^2 / (k(1 - R^2)), 
# where k = number of predictors, N = number of cases

# First model: (the first model mades R^2 change from 0 to 0.335
# and this change is F1 = 99.59)
album2Model
summary.lm(album2Model)
summary.aov(album2Model)
N = dim(album1)[1]; N
k1 = 1
R.squared1 = summary(album2Model)$r.squared
F1 = (N - k1 - 1) * R.squared1 / (k1*(1 - R.squared1)); F1

# Second model: (the addition of new predictors in the model 3
# makes R^2 increase an additional 0.330 and the F ratio for this
# relative change is = 96.44)
summary.lm(album3Model)
summary.aov(album3Model)
N = dim(album2)[1]; N
k2 = 3
k.change = k2 - k1
R.squared2 = summary(album3Model)$r.squared
R.squared.change = R.squared2 - R.squared1; R.squared.change
Fchange = (N - k2 - 1) * R.squared.change / (k.change*(1 - R.squared2)); Fchange
# degrees of freedom are: kchange = 2, N-k2-1 = 200-3-1 = 196
p.value = 1 - pf(Fchange, df1=k.change, df2 = (N-k2-1)); p.value
# The easy way: 
anova(album2Model, album3Model) # model1 must be subset of model2...
# CONCLUDE: model3 is vastly improved compared to model2 



# Testing accuracy of the model
# outliers: resid(), rstandard(), rstudent()
# influentials: cooks.distance(), dfbeta(), hatvalues(), covratio()

album2$resid = resid(album3Model)
album2$stz.r = rstandard(album3Model)
album2$stu.r = rstudent(album3Model)
album2$cooks = cooks.distance(album3Model)
album2$dfbeta = dfbeta(album3Model) # are the model diff without a yvalue
album2$dffit = dffits(album3Model)
album2$lev = hatvalues(album3Model)
album2$covratio = covratio(album3Model)
head(album2)

write.table(album2, "data/Album Sales With Diagnostics.dat", 
            sep="\t", row.names=FALSE)

#------------------------------------------------------------
# STANDARDIZED RESIDUALS

# 95% of cases have stz.r within +/- 2 
# so 5% are out of bound
mean(abs(album2$stz.r) > 2)
mean(abs(album2$stz.r) > 2.5)
which(abs(album2$stz.r) > 3) # this case is an outlier

# which values are out of bound?
album2$large.stz.r <- abs(album2$stz.r) > 2
head(album2)
album2[album2$large.stz.r, 
       c("sales", "airplay", "attract", "adverts", "stz.r")]


#--------------------------------------------------------
# COOKS, LEV, COVRATIO
album2[album2$large.stz.r, c("cooks", "lev", "covratio")]

  # which cooks values are not < 1 ?
large.cooks = album2$cooks > 1; mean(large.cooks)
album2[large.cooks, c("cooks")] # none are > 1. Yay!


  # leverages must be less than 0.02 or 0.04 or 0.06
    # lev.bound = (k + 1)/n
large.lev = album2$lev > 0.02
out.bound.lev1 = album2[large.lev, c("lev")]
# OR ask this way: 
indexes.lev = which(album2$lev > 0.02)
out.bound.lev2 = album2$lev[indexes.lev]
identical(out.bound.lev1, out.bound.lev2)
# how many (in percentage) are beyond 0.02? 
100*length(out.bound.lev1)/(dim(album2)[1]) #or the lev2 one


  # covratios must be within (0.94, 1.06)
  # lower = 1 - (3(k+1)/n) = 1-3(4)/200 = 0.94
  # upper = 1 + (3(k+1)/n) = 1.06
indexes.cov = which(album2$covratio > 1.06 | album2$covratio < 0.94)
out.bound.cov = album2$covratio[indexes.cov]
out.bound.cov # these values are problematic
100*length(out.bound.cov)/(dim(album2)[1]) 

#------------------------------------------------------------

# ASSESSING INDEPENDENT ERRORS (Durbin-Watson)

# pvalue is bootstrapped so it is not the same every time...
durbinWatsonTest(album3Model)
dwt(album3Model)
# CONCLUDE: closest to 2 is best, means errors are independent

#------------------------------------------------------------

# ASSESSING Multicollinearity (VIF)

  # largest VIF must be < 10
  # tolerance must not be below 0.1
  # regression is biased is avg VIF is much greater than 1
vif(album3Model) # vif
1/vif(album3Model) # tolerance
mean(vif(album3Model))
# CONCLUDE: no multicollinearity

#------------------------------------------------------------

# Checking Residuals Assumptions

# is fits vs stz residuals perfectly normal?
album2$fitted = album3Model$fitted.values
g = ggplot(data=album2, 
           aes(x=album2$fitted, 
               y=rstandard(album3Model))) + geom_point() +
  labs(x="Fitted values", y="Studentized Residuals")
# since these are fitted values, fitted line is a perfect mean
g + geom_smooth(method="lm") 

# are studentized residuals normal?
hist = ggplot(album2, aes(stu.r)) + 
  geom_histogram(aes(y=..density..), fill="white", col="grey")
curve = stat_function(fun=dnorm, 
                     args=list(
                       mean=mean(album2$stz.r,na.rm=T),
                       sd=sd(album2$stz.r,na.rm=T)),
                colour="blue", size=1)
hist + curve

# qqplot of studentized residuals
q = qplot(sample=album2$stu.r, stat='qq') + 
  labs(x="Theoretical stu.r", y="Observed stu.r"); q



#------------------------------------------------------------

# Bootstrapping Regression Coefficients

bootReg <- function(formula, data, indices){
  d = data[indices, c(1:4)]
  fit = lm(formula, data=d)
  return(coef(fit))
}
bootRegResults <- boot(data=album2, statistic=bootReg, 
                       formula=sales~adverts+airplay+attract,
                       R=2000)
res <- bootRegResults$t
head(res)
head(res[,1])
res <- data.frame(advertsCoeff=res[,1], salesCoeff=res[,2], 
           airplayCoeff=res[,3], attractCoeff=res[,4])
head(res)
colMeans(as.matrix(res))

# Conf ints for the sales (intercept)
boot.ci(bootRegResults, type="bca", index=2)
# Conf ints for adverts
boot.ci(bootRegResults, type="bca", index=1)
# Conf ints for airplay
boot.ci(bootRegResults, type="bca", index=3)
# Conf ints for attract
boot.ci(bootRegResults, type="bca", index=4)

# --> Conf ints are close to plug-in approach (output 7.4)
# suggesting that model is near normal



# -----------------------------------------------------------------

## UNDERSTANDING REGRESSION WITH FACTORS (using contrasts)
# question: how does this relate to regression with number vars?

# Dummy Variable Coding
gfr <- read.delim("data/GlastonburyFestivalRegression.dat", header=T)
# hygiene score 0 - 4 (stinky - clean)
# music - type of music people affiliate themselves with
head(gfr) 
# there are 4 music groups - need 3 dummy variables
# baseline - no musical affiliation - 0
# crusty, indie, metaller - 1 in the right places

# SET CONTRASTS AUTOMATICALLY
contrasts(gfr$music) <- contr.treatment(4, base=4)
head(gfr$music)
# SET MANUALLY
crustyCode <- c(1,0, 0, 0)
indieCode <- c(0, 1, 0 , 0)
metalCode <- c(0, 0, 1, 0)
contrasts(gfr$music) <- cbind(crustyCode, indieCode, metalCode)
head(gfr$music)

# Model with dummy variables
glastonburyModel <- lm(data=gfr, change ~ music)
summary.lm(glastonburyModel)
summary.aov(glastonburyModel)


# MEANING OF CONTRASTS

# Slopes = rel.diff between each group and baseline group
# Slopes (the change) is converted to t-statistic ==> significance

# Find mean of change variable for each music group
 # (splitting the change variable by music group)
round(tapply(gfr$change, gfr$music, mean, na.rm=TRUE), 3)
glastonburyModel
(-0.966 - (-0.554)) # crustyMean - noAffMean = crustySlope
(-0.964 - - 0.544) # indieMean - noAffMean = indieSlope
(-0.526 - - 0.554) # metallerMean - noAffMean = metallerSlope
# intercept = noAffMean

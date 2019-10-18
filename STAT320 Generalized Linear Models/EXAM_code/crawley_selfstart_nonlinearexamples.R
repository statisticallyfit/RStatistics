setwd("C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/stat320_rcode")
source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/FORMULAS.R')
source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/PLOTTING.R')
source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/Rfunctions.R')


library(ggfortify)


options(show.signif.stars = FALSE)


# (1) --------------------------------------------------------------------------------------

# Self-starting Michaelis-Menten Model: y = ax / (b + x)
# In R: SSmicmen
# INTUITION: reaction rate (y) is function of enzyme concentration
# and increases quickly but asymptotes once reaction rate is no longer
# enzyme-limited. 

enzymeData <- read.table("data/mm.txt", header=TRUE)
ggplot(enzymeData, aes(x=conc, y=rate)) + geom_point(size=3)

enzyme.nls <- nls(rate ~ SSmicmen(conc, a, b), data=enzymeData) # a, b are the parameters
summary(enzyme.nls)

# Plotting resulting equation with original graph: 
xs <- seq(0, 1.2, by=0.01)
preds <- predict(enzyme.nls, list(conc = xs))

#df.residual <- summary(enzyme.nls)$df[2]
#s <- summary(enzyme.nls)$sigma 

ci.df <- data.frame(conc=xs, 
                    #upr = preds + qt(0.975, df=df.residual) * sqrt(s^2 + var.atxstar),
                    #lwr = preds - qt(0.975, df=df.residual) * sqrt(s^2 + var.atxstar),
                    pred = preds)

ggplot(data=enzymeData, aes(x=conc, y=rate)) + geom_point(size = 2) + 
  geom_line(data=ci.df, aes(y = pred), color="dodgerblue", size=1) 
#geom_line(data=ci.df, aes(y=upr), linetype="dashed", color="red") +
#geom_line(data=ci.df, aes(y=lwr), linetype="dashed", color="red")



# (2) --------------------------------------------------------------------------------------

# Self-starting Asymptotic Exponential: y = a - be^(-cx)
# a = horizontal asymptote
# b = a - R0, where R0 = intercept (y(0))
# c = rate constant

jawData <- read.table("data/jaws.txt", header=TRUE)
jawData

jaw.nls <- nls(bone ~ SSasymp(age, a, b, c), data=jawData)
jaw.nls

# Plotting resulting equation with original graph: 
xs <- seq(0, 50, 0.2)
preds <- predict(jaw.nls, list(age = xs))
predDf <- data.frame(age=xs,  pred = preds)

ggplot(data=jawData, aes(x=age, y=bone)) + geom_point(size = 2) + 
  geom_line(data=predDf, aes(y = pred), color="dodgerblue", size=1) 


# (3) --------------------------------------------------------------------------------------

# Self-starting Logistic: 

logisticData <- read.table("data/sslogistic.txt", header=TRUE)
logisticData

logistic.nls <- nls(density ~ SSlogis(log(concentration), a, b, c), data=logisticData)
logistic.nls

# Plotting resulting equation with original graph: 
xs <- exp(seq(-3, 3, 0.1)) # or use range function to set from, to hre. 
preds <- predict(logistic.nls, list(concentration = xs))
predDf <- data.frame(concentration=xs,  pred = preds)

ggplot(data=logisticData, aes(y=density, x=log(concentration))) + geom_point(size = 2) + 
  #geom_point(data=logisticData, aes(y=density, x=concentration), size=2, color="magenta") +
  geom_line(data=predDf, aes(y = pred), color="dodgerblue", size=1) 


summary(logistic.nls)

# (4) --------------------------------------------------------------------------------------

# Self-starting four-parameter logistic: allows lowr and upper asymptotes

chickData <- read.table("data/chicks.txt", header=TRUE)
chickData

chick.nls <- nls(weight ~ SSfpl(Time, a, b, c, d), data=chickData)
chick.nls

# Plotting resulting equation with original graph: 
xs <- seq(0, 22, 0.2) # or use range function to set from, to hre. 
preds <- predict(chick.nls, list(Time = xs))
predDf <- data.frame(Time=xs,  pred = preds)

ggplot(data=chickData, aes(y=weight, x = Time)) + geom_point(size = 2) + 
  geom_line(data=predDf, aes(y = pred), color="dodgerblue", size=1) 


summary(chick.nls)

# (5) --------------------------------------------------------------------------------------

# Self-starting Weibull growth function: Asym - Drop*exp( - exp(lrc) * x^pwr)
# Asym = horizontal asymptote to the right
# Drop = difference between asymptote and intercept (value of y at x= 0)
# lrc = natural logarithm of the rate constant
# pwr = power to which x is raised. 

weibData <- read.table("data/weibull.growth.txt", header=TRUE)
weibData

weibull.nls <- nls(weight ~ SSweibull(time, Asymp, Drop, lrc, pwr), data=weibData)
summary(weibull.nls)

# Plotting resulting equation with original graph: 
xs <- seq(2, 22, 0.1) # or use range function to set from, to hre. 
preds <- predict(weibull.nls, list(time = xs))
predDf <- data.frame(time=xs,  pred = preds)

ggplot(data=weibData, aes(y=weight, x = time)) + geom_point(size = 2) + 
  geom_line(data=predDf, aes(y = pred), color="dodgerblue", size=1) 




# (6) --------------------------------------------------------------------------------------

# Self-starting first order compartment function
# y = k exp(-exp(a)x - exp(-exp(b)x)

# where k = Dose * exp(a + b - c)/(exp(b) - exp(a))

drugRespData <- read.table("data/fol.txt", header=TRUE)
drugRespData

drug.compartment.nls <- nls(conc ~ SSfol(Dose, Time, a, b, c), data=drugRespData)

setwd("C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/stat320_rcode")
#source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/FORMULAS.R')
#source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/PLOTTING.R')
#source('C:/D/mydocs/ANA_EXAMS_2019/STAT320_GeneralizedLinearModels/RCodeWork/Rfunctions.R')


library(ggfortify)


options(show.signif.stars = FALSE)


frogData <- read.table("data/CO.txt", header=TRUE)
head(frogData)


# part a) ---------------------------------------------------------------------------

# Plot the data, well label it, examine shape of data, 
# use this to find two plausible models. 
ggplot(frogData, aes(x=Frogs, y=Conc)) + geom_point(size=3) +
  ggtitle("Conc vs Frogs")

ggplot(data=frogData, aes(y=log(Conc), x=Frogs)) + geom_point(size = 2) +
  ggtitle("Log(Conc) vs Frogs")

#ggplot(data=frogData, aes(y=Conc, x=log(Frogs))) + geom_point(size = 2)



# DO: original data ---------------------------------------------------------------------

# Self-starting Logistic: 

frog.logistic.nls <- nls(Conc ~ SSlogis(Frogs, a, b, c), data=frogData)
frog.logistic.nls

summary(frog.logistic.nls)

# Plotting resulting logistic equation with original graph: -------------------
rng = range(frogData$Frogs)

xs <- seq(from=rng[1], to=rng[2], by=0.1) # or use range function to set from, to hre. 
preds <- predict(frog.logistic.nls, list(Frogs = xs))
predDf <- data.frame(Frogs=xs,  pred = preds)

ggplot(data=frogData, aes(y=Conc, x=Frogs)) + geom_point(size = 2) + 
  geom_line(data=predDf, aes(y = pred), color="dodgerblue", size=1) 


# DOING log(y), x------------------------------------------------------------------------

# Self-starting Asymptotic Exponential: y = a - be^(-cx)
# a = horizontal asymptote
# b = a - R0, where R0 = intercept (y(0))
# c = rate constant

frog.expAsymp.nls <- nls(log(Conc) ~ SSasymp(Frogs, a, b, c), data=frogData)
frog.expAsymp.nls

summary(frog.expAsymp.nls)

# Plotting resulting exponential equation with original graph:  ------------------
rng = range(frogData$Frogs)
xs <- seq(from=rng[1], to=rng[2], by=0.1)
preds <- predict(frog.expAsymp.nls, list(Frogs = xs))
predDf <- data.frame(Frogs=xs,  pred = preds)

ggplot(data=frogData, aes(x=Frogs, y=log(Conc))) + geom_point(size = 2) + 
  geom_line(data=predDf, aes(y = pred), color="dodgerblue", size=1) 



# part c) ---------------------------------------------------------------------------------

# Comparing model fit:

# Calculation of R^2 (variation explained)

calcR2ForNonlinear <- function(fit.nls, nullfit.lm){
  
  df.residual <- summary(fit.nls)$df[2]
  s <- summary(fit.nls)$sigma # called the standard error of the regr model
  # residual standard error
  SSE <- s^2 * df.residual
  
  # Fit the null model to get the total SST
  #jawNull.lm <- lm(bone ~ 1, data=jawData)
  df.total <- summary(nullfit.lm)$df[2]
  s <- summary(nullfit.lm)$sigma
  SST <- s^2 * df.total
  
  # R2
  return(1 - SSE/SST)
}



# R^2 prediction accuracy  +++++++++++++++++++++++++++++++++++++++++++++++++++++
null.logistic.lm <- lm(Conc ~ 1, data=frogData)

calcR2ForNonlinear(frog.logistic.nls, null.logistic.lm)
# R2 = 0.999 so 99% of variation in y explained by this model


null.exp.lm <- lm(log(Conc) ~ 1, data=frogData)
calcR2ForNonlinear(frog.expAsymp.nls, null.exp.lm)
# 0.94632366


# SUMMARY COEFF FITS ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
summary(frog.logistic.nls) # all predictors significant
summary(frog.expAsymp.nls)

# COMPARE GRAPHS

# Residuals +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Diagnostics: fitted vs residuals (for frog.logistic model)
df.logistic.diagnostics <- data.frame(res = residuals(frog.logistic.nls, type="pearson"), 
                                      fitted = predict(frog.logistic.nls))
# NOTE: the pearson residuals above ARE standardized. Check: 


# Residuals vs fitted. 
ggplot(df.logistic.diagnostics, aes(x=fitted, y=res)) + geom_point(size=3) + 
  geom_hline(yintercept=0, linetype="dashed", size=1,color="red") + 
  geom_hline(yintercept=c(-2,2), linetype="dotted", size=1, color="black") 

# QQnorm
ggplot(df.logistic.diagnostics, aes(sample = res)) + 
  stat_qq(color="dodgerblue", size=3) + 
  stat_qq_line(linetype="dashed", size=1)

shapiro.test(df.logistic.diagnostics$res)
# No non-normality


# ----------

# Diagnostics for the asymptotic exponential model
df.exp.diagnostics <- data.frame(res = residuals(frog.expAsymp.nls, type="pearson"), 
                                 fitted = predict(frog.expAsymp.nls))
# NOTE: the pearson residuals above ARE standardized. Check: 


# Residuals vs fitted. 
ggplot(df.exp.diagnostics, aes(x=fitted, y=res)) + geom_point(size=3) + 
  geom_hline(yintercept=0, linetype="dashed", size=1,color="red") + 
  geom_hline(yintercept=c(-2,2), linetype="dotted", size=1, color="black") 

# QQnorm
ggplot(df.exp.diagnostics, aes(sample = res)) + 
  stat_qq(color="dodgerblue", size=3) + 
  stat_qq_line(linetype="dashed", size=1)

shapiro.test(df.exp.diagnostics$res)
# # highly non normal


# part d) ---------------------------------------------------------------------------------------

## Plotting fitted vs conc
df <- data.frame(Conc=frogData$Conc, Pred=predict(frog.logistic.nls))
ggplot(data=df, aes(x=Conc, y=Pred)) + geom_point(size=2)









#--------------------------------------------------------------------------

## TERRIBLE 


# Self-starting Michaelis-Menten Model: y = ax / (b + x)
# In R: SSmicmen
# INTUITION: reaction rate (y) is function of enzyme concentration
# and increases quickly but asymptotes once reaction rate is no longer
# enzyme-limited. 

frog.menton.nls <- nls(log(Conc) ~ SSmicmen(Frogs, a, b), data=frogData)
frog.menton.nls

# Plotting resulting equation with original graph: 
rng <- range(frogData$Frogs)
xs <- seq(from = rng[1], to=rng[2], by=0.1)
preds <- predict(frog.menton.nls, list(Frogs = xs))
predDf <- data.frame(Frogs=xs,  pred = preds)

ggplot(data=frogData, aes(x=Frogs, y=log(Conc))) + geom_point(size = 2) + 
  geom_line(data=predDf, aes(y = pred), color="dodgerblue", size=1) 





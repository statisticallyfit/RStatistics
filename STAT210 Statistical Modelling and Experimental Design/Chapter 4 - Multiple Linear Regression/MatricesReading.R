
# Using R to fit SLR model 
# X [1,1,3,3]
# Y = [1,3,4,6]

options(digits=3)

X <- c(1,1,3,3)
Y <- c(1,3,4,6)

# Fit the linear model, and include the argument x = T to store
# the design matrix
xy.lm <- lm(Y ~ X, x=TRUE)
summary(xy.lm)

# Vector of fitted coefs B-hat, vector of fitted vals Y-hat, and 
# the design matrix X and residuals vector e-hat. 
xy.lm$coefficients
cbind(Y, xy.lm$fitted.values, xy.lm$residuals)
# Print the design matrix
xy.lm$x

# FItted values are obtained this way: Y-hat = X * B-hat
theYHats <- xy.lm$x %*% xy.lm$coef; theYHats
# these are equal to the fitted values
xy.lm$fitted.values

# Plotting
plot(X, Y)
abline(xy.lm)




# Case 2: predictor is a factor with levels that correspond
# to treatments
X <- c("A", "A", "B", "B")
g <- factor(X); g
Y <- c(1,3,4,6)
# fit the model and store design matrix
xy.lm <- lm(Y ~ g, x=TRUE)
summary(xy.lm)
cbind(Y, xy.lm$fitted.values, xy.lm$residuals)
# fitted model is Y-hat = X * B-hat
theYHats <- xy.lm$x %*% xy.lm$coef
theYHats
# Since they chose base level to be A (since they have gB), the intercepts
# are interpreted as: B0 = mean_A, B1 = mean_B - mean_A
# ((default base level is chosen alphanumerically, lesser letters
# go first as the base))
# Check Y = Y-hat + e-hat
Y #
theYHats + xy.lm$residuals

# Plotting
#plot(xy.lm$x, xy.lm$fitted.values)
xsAs01s <- matrix(xy.lm$x[,2])
plot(xsAs01s, Y)
abline(xy.lm)



# Case: Model without intercept, still factors
xy1.lm <- lm(Y ~ g - 1, x=TRUE)
summary(xy1.lm)
summary(xy.lm)
xy.lm
xy1.lm
xy1.lm$coefficients
cbind(Y, xy1.lm$fitted.values, xy1.lm$residuals)


# Comparing the design matrices for the two models!
xy.lm$x
xy1.lm$x
cbind(xy.lm$x, replicate(4, "dash"), xy1.lm$x)
# A = 0, B = 1
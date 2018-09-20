setwd("/development/projects/statisticallyfit/github/R/RStatistics/STAT270 Inference/ASSIGNMENTS")

options(show.signif.stars = FALSE)

# QUESTION 1:  ----------------------------------------------------------------------

# part a) calculate correlation coefficient
x = c(65, 63, 67, 64, 68, 62, 70, 66, 68, 67, 69, 71)
y = c(68, 66, 68, 65, 69, 66, 68, 65, 71, 67, 68, 70)

n = length(x)
Sxx = sum(x^2) - (1/n) * sum(x)^2; Sxx
# [1] 84.66667
Syy = sum(y^2) - (1/n) * sum(y)^2; Syy
# [1] 38.91667
Sxy = sum(x*y) - (1/n) * sum(x) * sum(y); Sxy
# [1] 40.33333

r = Sxy / sqrt(Sxx * Syy); r
# [1] 0.7026516

# Checking with built-in function: 
cor(x, y)
# [1] 0.7026516 

# the result is the same

# Testing hypothesis H0: rho = 0

# calculate the test-statistic: 
t = r * sqrt(n-2) / sqrt(1 - r^2); t
# [1]3.122802

# use alpha = 0.05 as significance level, and this is a two-sided test so 
# calculate the critical t-value at area 0.025. 
t.crit = abs(qt((1-0.95)/2, df=n-2)); t.crit
# [1] 2.228139

p.value = 2 * pt(t, df=n-2, lower.tail=F); p.value
# [1] 0.01082225


# part b) find  correlation coeffiicent at rejection region, given n = 32
t.crit = abs(qt(0.025, df=32-2)); t.crit
# [1] 2.042272

r.crit = t.crit / sqrt(t.crit^2 + 32 - 2); r.crit
# [1] 0.34937
# Any lower than 0.349 and the null hypothesis of rho = 0 would not be rejected. 



# QUESTION 2: ----------------------------------------------------------------------

# part a)
x = c(-3,-2,-1,0,1,2,3)
y = c(1,0,0,  -1,-1,0,0)
n = length(x)
data = data.frame(x=x,y=y, x2=x^2)
k = 2 # number of beta parameters not including the intercept. 

model.lm <- lm(y ~ x + x2, data=data)
summary(model.lm)

# Y-hat = -0.71429 -0.1429 x + 0.1429 x^2

# part b) 
Syy = sum(y^2) - (1/n) * sum(y)^2
X = matrix(c(rep(1, n), x, x^2), ncol=3)
Y = matrix(y, ncol=1)
beta.hat = solve(t(X) %*% X) %*% (t(X) %*% Y); beta.hat
SSE = t(Y) %*% Y - t(beta.hat) %*% (t(X) %*% Y); SSE # need the above calculations to get SSE

Fstat = ((Syy - SSE) / k) / (SSE / (n - k -1)); Fstat
p.value = pf(Fstat, df1=k, df2=n-k-1, lower.tail=F); p.value

summary(model.lm)

# The p.value = 0.04 < 0.05, so we can reject null hypothesis and say there is
# evidence that at least one slope parameter (either B1 or B2 or both) is not zero. 
# R^2 = (multiple R squared) = 0.8 means about 80% of variation in Y can be
# explained by the regression model. (strong relation)


# part c) 
# design matrix


X
#      [,1] [,2] [,3]
# [1,]    1   -3    9
# [2,]    1   -2    4
# [3,]    1   -1    1
# [4,]    1    0    0
# [5,]    1    1    1
# [6,]    1    2    4
# [7,]    1    3    9



# QUESTION 3: --------------------------------------------------------------------
# part a)
# R = clearcut, T = thinned, C = control
# blocks = F1, F2, F3 (the forest stands)
# treatments = clearcut, thinned, control. 
treeData <- data.frame(Y=c(4.3,10.8,8, 12.2,14.1,16.5, 8.7,11.4,14.3),
                       Treatment=c(rep("R",3),rep("T",3),rep("C",3)),
                       Block=c(rep(c("F1","F2","F3"),3)),stringsAsFactors = TRUE)
treeData
#     Y Treatment Block
# 1  4.3         R    F1
# 2 10.8         R    F2
# 3  8.0         R    F3
# 4 12.2         T    F1
# 5 14.1         T    F2
# 6 16.5         T    F3
# 7  8.7         C    F1
# 8 11.4         C    F2
# 9 14.3         C    F3



# part b)
tree.lm <- lm(Y ~ Block + Treatment, data=treeData)
summary(tree.lm)
anova(tree.lm)

# Can reject null hypothesis of no treatment effect since p-value F = 0.02 < 0.05

# part c) 95% CI for difference between treatment clearcut "R" and thinned "T"

# calculating the Treatment means: 
tbl = with(treeData, tapply(Y, list(Treatment), mean))

mean.clearcut = tbl[[2]]; mean.clearcut
# [1] 7.7
mean.thinned = tbl[[3]]; mean.thinned
# [1] 14.26667

# Confidence interval (95%)
n = nrow(treeData)
b = 3 # number of blocks
k = 3 # number of treatments
anv = anova(tree.lm)
s = sqrt(anv$`Mean Sq`[3]) # s is the square root of the MSE
t.crit = abs(qt(0.025, df=n - b - k + 1)); t.crit

CI.treeDiff = (mean.clearcut - mean.thinned) + c(-1,1) * t*s*sqrt(2/b)
CI.treeDiff
# [1] -7.492487 -5.640847



# TODO: check section 13.9 wackerly if needed, but also lecture 7b contrasts?



# --------------------------------------- ERASE LATER -----------------------
# testing - wackerly grass 13.46
grassData = data.frame(Y=c(2.764, 2.568, 2.506, 2.612,2.238,
                           3.043,2.977,2.533,2.675,2.616,
                           2.6,2.183,2.334,2.164,2.127,
                           3.049,3.028,2.895,2.724,2.697),
                       Block=c(rep("1",5),rep("2",5),rep("3",5),rep("4",5)),
                       Treatment=c(rep(c("A","B","C","D","E"),4)),stringsAsFactors = TRUE)
grassData
grass.lm <- lm(Y ~ Block + Treatment, data=grassData)
anova(grass.lm)

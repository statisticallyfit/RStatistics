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
# A = clearcut, B = thinned, C = control
# blocks = F1, F2, F3 (the forest stands)
# treatments = clearcut, thinned, control. 
treeData <- data.frame(Y=c(4.3,12.2,8.7, 10.8,14.1,11.4, 8,16.5,14.3),
                       Block=c(rep("F1", 3), rep("F2", 3), rep("F3",3)), 
                       Treatment=c(rep(c("A","B","C"),3)), stringsAsFactors = TRUE)
treeData
#      Y Block Treatment
# 1  4.3    F1         A
# 2 12.2    F1         B
# 3  8.7    F1         C
# 4 10.8    F2         A
# 5 14.1    F2         B
# 6 11.4    F2         C
# 7  8.0    F3         A
# 8 16.5    F3         B
# 9 14.3    F3         C


# part b)
tree.lm <- lm(Y ~ Block + Treatment, data=treeData)
summary(tree.lm)
anova(tree.lm)

# Analysis of Variance Table
# Response: Y
#          Df Sum Sq Mean Sq F value  Pr(>F)
# Block      2 34.936  17.468  6.1845 0.05971
# Treatment  2 65.149  32.574 11.5330 0.02184
# Residuals  4 11.298   2.824  


# Can reject null hypothesis of no treatment effect since p-value F = 0.0218 < 0.05



# part c) 95% CI for difference between treatment clearcut "A" and thinned "B"

# Confidence interval (95%)
n = nrow(treeData)
b = 3 # number of blocks
k = 3 # number of treatments

# calculating the Treatment means:
treeBoxData <- matrix(c(4.3,10.8,8, 12.2,14.1,16.5, 8.7,11.4,14.3),ncol=3)
colnames(treeBoxData) <- c("F1", "F2", "F3")
rownames(treeBoxData) <- c("A", "B", "C")

# Creating a function to return the data with the row and column sums
marginalTable <- function(tbl) {
      extraRow <- cbind(tbl, RowTotals=margin.table(tbl, margin=1))
      extraCol <- rbind(extraRow, ColTotals=margin.table(extraRow, margin=2))
      
      dms <- dimnames(tbl)
      dms[[1]] <- c(dms[[1]], "ColTotals")
      dms[[2]] <- c(dms[[2]], "RowTotals")
      dimnames(extraCol) <- dms 
      
      return(extraCol) 
}

mgTbl <- data.frame(marginalTable(treeBoxData))

mean.clearcut = mgTbl$RowTotals[1] / b; mean.clearcut
# [1] 8.4
mean.thinned = mgTbl$RowTotals[2] /b; mean.thinned
# [1] 12.1

# OR can do rowMeans:
rowMeans(treeBoxData)
#    A        B        C 
# 8.40000 12.10000 12.93333 


# Locating the s = sqrt(MSE)

anovaTestObj = anova(tree.lm); anovaTestObj
# Analysis of Variance Table
# Response: Y
#           Df Sum Sq Mean Sq F value  Pr(>F)
# Block      2 34.936  17.468  6.1845 0.05971
# Treatment  2 65.149  32.574 11.5330 0.02184
# Residuals  4 11.298   2.824 

s = sqrt(anovaTestObj$`Mean Sq`[3]); s # s is the square root of the MSE
# [1] 1.680608
t.crit = abs(qt(0.025, df=n - b - k + 1)); t.crit

CI.treeDiff = (mean.clearcut - mean.thinned) + c(-1,1) * t.crit*s*sqrt(2/b)
CI.treeDiff
# [1] -7.5098684  0.1098684




# QUESTION 3 c) -------------------------------------------------------------

alpha = 0.1
beta = 10
ys <- c(3,9,5,2,2,5,2,4,2,3)
n = length(ys)
u = sum(ys); u
# [1] 37 

alpha.star = alpha + u; alpha.star
# [1] 37.1
beta.star = beta/(n*beta + 1); beta.star
# [1] 0.0990099

a = qgamma(0.025, shape=alpha.star, scale=beta.star); a
# [1] 2.587652
b = qgamma(0.025, shape=alpha.star, scale=beta.star, lower.tail=FALSE); b
# [1] 4.946071
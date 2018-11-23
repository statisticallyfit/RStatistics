# to compare tests: statpages.org/ctab2x2.html
# good resource: http://www.stat.wisc.edu/~st571-1/06-tables-4.pdf
setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/StatisticsIntroUsingR_Crawley")



# Comparing Variances


# Fisher's F Test used to compare two variances
# Fisher's Exact Test used do in place of chi square when cells are < 5

# critical value
f.crit <- qf(0.975, df1=9, df2=9)
f.test.data <- read.csv("data/f.test.data.csv")
attach(f.test.data)
f.test.data
f.ratio <- var(gardenC)/var(gardenB)
# since f.ratio > f.crit , reject null that ratio is not 1
p.value <- 2*(1-pf(f.ratio, df1=9, df2=9))

# --> NOTE: since variances are significantly different, don't compare means

#same
var.test(gardenC, gardenB)
var.test(gardenB, gardenC)

detach(f.test.data)





# Comparing Means

#   (1) Student's t Test when samples are independent, variances are constant, and errors are normal
#   (2) Wilcoxon rank-sum test when samples are independent, variances are constant, but errors are not normal
# other tests for when variances are different

t.test.data <- read.csv("data/t.test.data.csv")
attach(t.test.data)
t.test.data
t.crit <- qt(0.975, df=18) # df= n1 + n2-2 = 10 + 10 - 2= 18

ozone <- c(gardenA, gardenB)
label <- factor(c(rep("A", 10), rep("B", 10)))
boxplot(ozone ~ label, notch=TRUE, xlab="Garden", ylab="Ozone pphm", col="cornflowerblue")
# because notches do not overlap, medians are significantly different at the 5% level

# t-test the long way
s2A <- var(gardenA)
s2B <- var(gardenB) 
s2A/s2B # test they are similar
nA <- length(gardenA)
nB <- length(gardenB)
t.statistic <- (mean(gardenA)-mean(gardenB))/(sqrt(s2A/nA + s2B/nB))
p.value <- 2*pt(-abs(t.statistic), df=(nA+nB-2))
p.value

# t-test the short way (Welch two sample t-test)
t.test(gardenA, gardenB)


# Wilcoxon Rank Sum Test: use if errors are not normal 
ozone <- c(gardenA, gardenB)
ozone
label <- factor(c(rep("A", 10), rep("B", 10)))
label
combined.ranks <- rank(ozone)
combined.ranks
# now find sum of ranks for each garden
ranksums <- tapply(combined.ranks, label, sum)
# the smaller of two ranks is the W statistic
W.statistic <- min(ranksums)
W.statistic
W.crit <- qwilcox(0.975, 10, 10)
p.value <- 2* (1-pwilcox(W.statistic, m=10, n=10))
p.value

# this uses normal approximation
# wilcox test is more conservative than t-test and is 95% as powerful with normal errors compared to t test
wilcox.test(gardenA, gardenB, correct=FALSE)
detach(t.test.data)


# Paired Samples
# NOTE: test is more accurate if variances are smaller (page 97)
stream <- read.csv("data/streams.csv")
attach(stream)
stream
t.test(down, up) # assuming independence, there is no sewage outfall impact on biodiversity score
t.test(down, up, paired=TRUE) # but if paired, there is significant impact
t.test(up-down) # test on differences of pairs
detach(stream)

# Binomial Test: 
# how likely is it to have 8 of 9 better on the new regime if there is no difference in the training regimes?
binom.test(1,9)
binom.test(8,9)$p.value 
#conclude that new regime is better than old one

p.value <- 2*(1-pbinom(7, size=9, prob=0.5))
p.value


# Binomial Test for Two Proportions
prop.test(c(4, 196), c(40, 3270)) # 4/40 women and 196/3270 men promoted - significant difference?



# Chisquared Test
chi.crit <- qchisq(0.95, df=1) # chi squared tests are always right-tailed
chi.crit
count <- matrix(c(38, 14, 11, 51), nrow=2)
rownames(count) <- c("Fair", "Dark")
colnames(count) <- c("Blue", "Brown")
count
res <- chisq.test(count, correct=F)
res
res$observed
res$expected




# Fisher's Exact Test: when one or more expected frequencies <5
# f.exact.statistic = the probability of any one particular outcome
# formula = (a+b)!(c+d)!(a+c)!(b+d)!/(a!b!c!d!n!)

# first make the table:
makeTable <- function(values, colNames, rowNames){
  table <- matrix(values, byrow=T, nrow=2)
  
  table <- cbind(table, c(sum(table[1,]), sum(table[2,])))
  colnames(table) = c(colNames, "ROW TOTALS")
  
  table <- rbind(table, c(sum(table[,1]), sum(table[,2]), sum(table[,3])))
  rownames(table) = c(rowNames, "COL TOTALS")
  
  table
}

# Method 1 for pvalue
situation1 <- makeTable(c(6,2,4,8), c("Tree A", "Tree B"), c("With ants", "Without ants"))
num <- factorial(8)*factorial(12)*factorial(10)*factorial(10)
p1 <- num/(factorial(6)*factorial(4)*factorial(2)*factorial(8)*factorial(20))
p1

# more extreme case: there could have been 1 tree with ants, so the other numbers are shifted
situation2 <- makeTable(c(7,1,3,9), c("Tree A", "Tree B"), c("With ants", "Without ants"))
p2 <- num/(factorial(7)*factorial(1)*factorial(3)*factorial(9)*factorial(20))
p2

# the last extreme case: no ants on tree B
situation3 <- makeTable(c(8,0,2,10), c("Tree A", "Tree B"), c("With ants", "Without ants"))
situation3
p3 <- num/(factorial(8)*factorial(2)*factorial(10)*factorial(20))
p3
p.value = 2*(p1+p2+p3) # doubled since it could have been tree A with fewer ants
p.value


# Method 2 for p value
values <- matrix(c(6,2,4,8), byrow=T, nrow=2)
values
fisher.test(values)


# Method 3 for p value
2*sum(dhyper(6:8, m=8, n=12, k=10)) #x, m, n, k

# Figure this out (pdf at top of page)
cows.bats <- makeTable(c(15,6,7,322), c("In estrous", "Not in estrous"), c("bitten by bat", "not bitten by bat"))
cows.bats
# these two are complements
sum(dhyper(15:21, m=21, n=329, k=22))
sum(dhyper(6:21, m=21, n=329, k=328))


# MEthod 4
fisher.test(matrix(c(6,2,4,8), byrow=T, nrow=2))


# nicer same way
table <- read.csv("data/fisher.csv")
table
attach(table)
fisher.test(tree, nests)
detach(table)




# Correlation and Covariance

# apply to continuous variables
# r = cov(x,y)/sqrt(var(x)var(y))
# cov(x,y) = Exp((x-mean(x))(y-mean(y)))

data <- read.csv("data/twosample.csv")
attach(data)
data
plot(x,y, pch=19, col="orange")
cov(x,y)
var(x,y)
r <- cov(x,y)/sqrt(var(x)*var(y))
r
cor(x,y)
detach(data)

# EXAMPLE
paired <- read.csv("data/water.table.csv")
attach(paired)
paired
# is there correlation between summer and winter table depths across these locations A - I?
cor(Summer, Winter)
plot(Summer, Winter, pch=19, col="violet")
cor.test(Summer, Winter) # correlation is significant

# Calculating correlation the hard way - same as cor(x,y)
varS <- var(Summer)
varW <- var(Winter)
varD <- var(Summer-Winter)
# r = (var(y) + var(z) - var(y-z))/(2*sqrt(var(y)*var(z)))
r = (varS + varW - varD)/(2*sqrt(varS * varW))
r
# are the samples independent? If so, then this is TRUE
varD
varS + varW
detach(paired)


# Scale-Dependent correlations
data <- read.csv("data/productivity.csv")
attach(data)
data
plot(productivity, mammals, pch=16, col="blue")
cor(productivity, mammals)
cor.test(productivity, mammals)
cor.test(productivity, mammals, method="spearman")
cor.test(productivity, mammals, method="kendall")

# but prod and mammals are negatively correlated when viewing the different species
plot(productivity, mammals, pch=16, col=as.numeric(region))
detach(data)

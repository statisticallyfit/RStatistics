# ONE SAMPLE t TEST

daily.intake = c(5260,5470,5640,6180,6390,6515, 6805,7515,7515,8230,8770)
mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)

" H0: mu = 7725
  H1: mu != 7725 "
infoLess = t.test(daily.intake, mu=7725, alternative="less") 
infoGreater = t.test(daily.intake, mu=7725, alt="g")
infoLess$p.value + infoGreater$p.value
infoLess$statistic == infoGreater$statistic

t.test(daily.intake, mu=7725, conf.level=0.98)
t.test(daily.intake, mu=7725)



# TWO SAMPLE t TEST
library(ISwR)
attach(energy)
energy
attributes(energy)

t.test(expend~stature) #expend is described(~) by stature
t.test(expend~stature, var.equal=T) # textbook two sample t test


# COMPARISON OF VARIANCES
"implements F test on ratio of group variances to test if variances
 are the same in two-sample t test. 
Assumes independent groups. "
var.test(expend~stature)


# PAIRED t TEST
attach(intake)
intake #pre/post menstrual energy intake
post-pre
t.test(pre, post, paired=TRUE)





# ONE SAMPLE WILCOXON TEST 
wilcox.test(daily.intake, mu=7725)
wilcox.test(daily.intake, mu=7725, alt="less", correct=FALSE)


# TWO SAMPLE WILCOXON TEST
wilcox.test(expend~stature)
# W test statistic = sum of ranks of first group - theoretical min


# MATCHED PAIRS WILCOXON TEST (same as one-sample Wilcoxon and t.test)
wilcox.test(pre, post, paired=TRUE)




# **************************** EXERCISES ************************

# 1
react
par(mfrow=c(1,1))
hist(react)
boxplot(react)
qqnorm(react)

t.test(react) # yes mean is significantly different than 0


# 2
attach(vitcap)
hist(vitcap$vital.capacity) #how to graph only group1 or group3?
t.test(vital.capacity~group, conf=0.99)


# 3
wilcox.test(react)
wilcox.test(vital.capacity~group, data=vitcap)


# 4 
intake
attach(intake)
opar <- par(mfrow=c(1,1))
# post vs pre plot
plot(post~pre); abline(0,1) # y ~ x (y is described by x)
# Bland-Altman plot: if dispersion changes with the level, 
# (ex if std is proportional to level, do log transformation)
plot((post+pre)/2, post-pre, ylim=range(0, post-pre)); abline(h=0)
# histogram
hist(post-pre)
# QQ plot
qqnorm(post-pre)
detach(intake)
par(opar)


# 5
par (mfrow=c(1,1))
shapiro.test(react)
qqnorm(react)
# react is 1-334, and shapiro helps remove outliers, which are last and first
shapiro.test(react[-c(1, 334)]) #means do test without the outliers
qqnorm(react[-c(1,334)])


# 6     ??????
ashina
t.test(ashina)
attach(ashina)
t.test(vas.active, vas.plac, paired=TRUE)
t.test((vas.active-vas.plac)[grp==1], (vas.plac-vas.active)[grp==2])


# 7
t.test(rnorm(25))$p.value
t.test(rt(25, df=2))$p.value
t.test(rexp(25), mu=1)$p.value
# repeating 10 times
x <- replicate(5000, t.test(rexp(25), mu=1)$p.value)
qqplot(sort(x), ppoints(5000), type='l', log='xy')
ppoints(5000)

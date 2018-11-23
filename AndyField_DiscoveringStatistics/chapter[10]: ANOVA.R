# data site: http://studysites.uk.sagepub.com/dsur/study/articles.htm

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstats/andyfieldbookcode")
library(ggplot2)

viagraData <- read.delim("data/Viagra.dat", header=TRUE)
viagraData
# plot
scatter <- ggplot(aes(x=person, y=libido, colour=dose), data=viagraData)
scatter + geom_point(shape=19)

# viagra data in dummy-code form
dummyData <- read.delim("data/Dummy.dat", header=TRUE)
dummyData



# Contrasts: 
# intercept = mean of placebo group = 2.2000 (H0 is probably that it is different from zero)
# dummy1 = mean of high dose - mean of placebo = 5 - 2.2 = 2.8000 = b2 (H0 is that the mean difference is 0, and same for b1)
# dummy2 = mean of low dose - mean of placebo = 3.2-2.2 = 1.0000 = b1

# Logic of the test: if the null is true and all groups 
# have same means, then b1 and b2 should be zero

dummyModel <- lm(libido~dummy1+dummy2, data=dummyData)
summary(dummyModel)
summary.aov(dummyModel)


# CALCULATE the long way:

# ~ Sums of squares

# SSM = sums of squares between groups 
# = SUM(n_k * (mean_k - mean_grand)^2)
# (how much of the variation the model explains)
# df_M = numgroups - 1 = k-1 = 3-1 = 2
SSM = 5*(2.2 - 3.467)^2 + 5*(3.2 - 3.467)^2 + 5*(5 - 3.467)^2
df_M = 3-1 

# SSR = sums of squares within groups 
# = SUM(x_ik - mean_k)^2
# = SUMs_k^2(n_k - 1)
# (how much of the variation the model does not explain)
# (intuitively: variation caused by extraneous/individual factors)
# df_R = N - k = numpeople - numgroups 
SSR = 1.7*(5-1) + 1.7*(5-1) + 2.5*(5-1)
df_R = 15-3

# ~ Mean squares

# MSM = SSM/df_M
MSM = SSM/df_M
# MSR = SSR/df_R
MSR = SSR/df_R

# ~ F-ratio
f.ratio = MSM/MSR

# ~ p-value
p.value = 1 - pf(f.ratio, df1=df_M, df2=df_R)


# how to change factor names
viagraData$dose = gl(3, 5, labels=c("placebo", "low", "high"))
viagraData$dose

dose = viagraData$dose; libido = viagraData$libido
viagraDataFrame = data.frame(dose, libido)
viagraDataFrame

" SITUATIONS: 

 (1) if there is one factor with 3 or more levels, use one-way ANOVA
 (2) if 1 factor with 2 levels, use t-test (F = t^2)
 (3) if 2 or more factors, use two or three way ANOVA  
 (4) factorial design: used if there is replication at each level 
in a multi-way ANOVA to test if the response to one factor depends 
on the level of another factor. "

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstats/crawleybookcode")

# (1) ONE-WAY ANOVA
oneway <- read.csv("data/oneway.csv")
attach(oneway)
oneway

plot(1:20, ozone, ylim=c(0,8), ylab="y", xlab="order", pch=19, col="red")
abline(h=mean(ozone), col="blue")
# segments
for(i in 1:20)
  lines(c(i, i), c(mean(ozone), ozone[i]), col="green")

# look at departure of data from means of Garden A and B
plot(ozone, ylim=c(0, 8), ylab="y", xlab="order", pch=19, bg=as.numeric(garden))
abline(h=mean(ozone[garden=="A"]))
abline(h=mean(ozone[garden=="B"]), col="red")

# segments
# this code with index[i] works since data was plotted in order
index <- 1:length(ozone)
for(i in 1:length(index)) {
  if(garden[i] == "A")
    lines(c(index[i], index[i]), c(mean(ozone[garden=="A"]), ozone[i]))
  else
    lines(c(index[i], index[i]), c(mean(ozone[garden=="B"]), ozone[i]), col="red")
}


# ANOVA analysis: is this difference in mean ozone in A and B significantly big? Or could it have occurred by chance alone?
# IMPORTANT: if means are significantly different, then variation within sample is smaller than variation between means of all samples. 

# SSE = variation within the sample (made of many SSEs for each sample)
# SSA = variation between each sample mean
# SSY = SSA + SSE
SSY <- sum((ozone - mean(ozone))^2); SSY
SSE <- 
  sum((ozone[garden=="A"] - mean(ozone[garden=="A"]))^2) +
  sum((ozone[garden=="B"] - mean(ozone[garden=="B"]))^2); SSE
SSA <- SSY - SSE; SSA

# build ANOVA table
varY <- SSY/19; varY # total
varE <- SSE/18; varE # error
varA <- SSA/1; varA # garden

"
 H0: mu1 = mu2  
 H1: at least one mean from the sample groups is different"
# If the variation between sample means is significantly greater 
# than variation within each sample, then the  means must be 
# different. 
f.ratio <- varA/varE; f.ratio
1 - pf(f.ratio, df1=1, df2=18) # so, the means are different

# Do the easy way
summary(aov(ozone~garden))
summary(lm(ozone~garden))
# df residuals = k(n-1) = (2 garden levels) * (10 replicates per garden - 1) = 2(10-1) = 2* 9 = 18


# Do graphical check of assumptions of model (constant variance and normal errors)
plot(aov(ozone~garden))
# first plot - shows that variances are identical in the two treatments
# second plot: values within each treatment group are normal
# third plot: residuals show constant variance
# fourth plot: attention to the values with large residuals




# Finding SSA directly (shortcut)
cbind(ozone[garden=="A"], ozone[garden=="B"])
tapply(ozone, garden, sum)
T1 <- 30; T2 <- 50

# shortcut SSA = sum(Ti^2)/n - (sum(y))^2/(kn)
SSA <- (30^2 + 50^2)/10 - (sum(ozone))^2/(2*10); SSA

# long way SSA <- n * sum((individual means - overall mean)^2)
SSA <- 10 * sum((mean(ozone[garden=="A"]) - mean(ozone))^2 + 
           (mean(ozone[garden=="B"]) - mean(ozone))^2); SSA

# SSY = sum((y - overall mean)^2)
SSY <- sum((ozone - mean(ozone))^2); SSY

# SSE = sum((y - individual mean)^2) for each of the k levels in the factor
SSE




# Effect sizes (treatment contrasts)
# a = overall mean, c = meangardenB - overall mean, b = meangardenA - overall mean. 
a = mean(ozone); a
# treatment contrast procedure: the factor level that comes first in alphabet is set equal to the intercept. The other parameters are differences between this mean and other relevant means. 
intercept <- mean(ozone[garden=="A"]); intercept
slope <- mean(ozone[garden=="B"]) - mean(ozone[garden=="A"]); slope

SEintercept <- sqrt(var(ozone[garden=="A"])/10); SEintercept
SEslope <- sqrt(var(ozone[garden=="B"])/10 + var(ozone[garden=="A"])/10); SEslope

summary(lm(ozone~garden))
summary.lm(aov(ozone~garden)) # these two lines are same things

# INTERPRET: 
# intercept is 3 = mean for garden A because A comes before B in alphabet
# slope is 2 = so the mean ozone in gardenB is 2 pphm higher than in garden A
# so mean for garden B = slope + intercept 
# could find means this way: 
tapply(ozone, garden, mean)

detach(oneway)





# Plots for Interpreting one way ANOVA - box plot or barplot with error bars
comp <- read.csv("data/competition.csv")
comp
attach(comp)

# boxplot
plot(clipping, biomass, xlab="Competition treatment", ylab="Biomass", col="dodgerblue", notch=TRUE)

# barplot
heights <- tapply(biomass, clipping, mean)
barplot(heights, col="green", ylim=c(0,700), ylab="mean biomass", xlab="competition treatment")

error.bars <- function(y, error) {
  x <- barplot(y, plot=FALSE) # finds x-coordinate of bar centers
  numErrorBars <- length(y)
  for(i in 1:numErrorBars)
    arrows(x[i], y[i]-error, x[i], y[i]+error, code=3, angle=90, length=0.15)
}
# code=3 means draw heads at both ends of the arrow
# y = the list of bar lengths
# length=0.15 means to shorted the heads of the error bars


# Find what error values to use for bar lengths
# (1) SE mean approach: even if they do not overlap, we cannot be sure the means are signficantly different (must pass a higher threshold)
model <- aov(biomass~clipping)
summary(model)
table(clipping)
# so the standard error of the mean = sqrt(var of residuals/6)
SEmean <- sqrt(4961/6); SEmean
se <- rep(SEmean, 5) #rep 5 times since there were 5 factors

error.bars(heights, se)

# (2) confidence interval approach: even if they do overlap, they may still be significantly different (must pass lower threshold)
ci <- SEmean*qt(0.975, df=5)
barplot(heights, col="green", ylim=c(0,700),
        ylab="mean biomass", xlab="competition treatment")
error.bars(heights, ci)
# now all bars overlap, so no significant difference between means (understand more)


# (3) LSD approach

# SE mean approach: means are NOT different if bars overlap, guaranteed
# CI approach: means ARE different when bars do not overlap
#LSD method: best of both: means ARE different when bars do not overlap AND means are not different when bars do overlap
lsd <- qt(0.975, df=10) * sqrt(2*4961/6); lsd # the two means are significantly different if they differ by 90.61 or more
lsd.bars <- rep(lsd, 5)/2; lsd.bars

barplot(heights, col="green", ylim=c(0,700),
        ylab="mean biomass", xlab="competition treatment")
error.bars(heights, ci)

detach(comp)



# Factorial Experiments
# have two or more factors, each with two or more levels with replication for each combination of factor level
# goal is to find if factors depend on each other

weights <- read.csv("data/growth.csv")
weights
attach(weights)

# add legend
labels <- levels(diet); labels
shade <- c(0.2, 0.6, 0.9)
# R puts the legend using the top left hand corner of the box of the legend
barplot(tapply(gain, list(diet, supplement), mean), beside=TRUE, 
        ylab="Weight gain", xlab="Supplement", ylim=c(0,30))
legend(locator(1), labels, gray(shade)) 

# inspect the mean values
tapply(gain, list(diet, supplement), mean)
gain ~ diet + supplement + diet:supplement
model <- aov(gain ~ diet*supplement)
model
summary(model)

# draw error bars for the grouped bars
tapply(gain, list(diet, supplement), length)
stderror <- sqrt(1.72/4)
x <- as.vector(barplot(tapply(gain, list(diet, supplement), mean), beside=TRUE, ylim=c(0,30)))
y <- as.vector(tapply(gain, list(diet, supplement), mean))
z <- rep(stderror, length(x))
for(i in 1:length(x)){
  arrows(x[i], y[i] - z[i], x[i], y[i]+z[i], length=0.05, code=3, angle=90)
}
legend(locator(1), labels, gray(shade))


# ANOVA doesn't show effect sizes
summary.lm(model) # this is better, but still complicated...
# model simplification
model2 <- lm(gain ~ diet + supplement)
summary(model2)

# since the supplements aren't significantly different from each other, reduce them
# agrimore and supersupp == best, control and supergain == worst
supp2 <- factor(supplement); supp2
levels(supp2)
levels(supp2)[c(1,4)] <- "best"
levels(supp2)[c(2,3)] <- "worst"
levels(supp2) # see the result of renaming

# now fit the simpler model (model3)
model3 <- lm(gain ~ diet+supp2)
anova(model2, model3) # compare the two models
# interpret: model3 has saved 2 df and is not significantly worse than the more complex model2 (since p = 0.158)

summary(model3)

detach(weights)





# SPlit plot experiments

# crop yield (levels: irrigated, not irrigated)
# sowing density (levels: low, medium, high)
# fertilizer application (levels: low, medium, high)

yields <- read.csv("data/splityield.csv")
attach(yields)
yields

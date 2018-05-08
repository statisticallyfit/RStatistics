setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/DRINKERS.Rdata")


# complete randomized design model
drinkers.lm <- lm(SCORE ~ GROUP, data=DRINKERS)
summary(drinkers.lm)
anova(drinkers.lm)

# F p-value is low so reject H0 that means are the same - conclude
# at least 2 means differ. (the global F-test is the same thing as testing
# that means are the same for a complete randomized design)



# calculating anova by hand
# 1) getting treatment totals T1, T2...
subset(DRINKERS, GROUP == "AR") # need whitespace func
DRINKERS <- removeWhitespace(DRINKERS, colsToFix = "GROUP")

totalAR <- sum(subset(DRINKERS, GROUP == "AR")$SCORE)
nAR <- nrow(subset(DRINKERS, GROUP=="AR"))
totalAC <- sum(subset(DRINKERS, GROUP == "AC")$SCORE)
nAC <- nrow(subset(DRINKERS, GROUP=="AC"))
totalA <- sum(subset(DRINKERS, GROUP == "A")$SCORE)
nA <- nrow(subset(DRINKERS, GROUP=="A"))
totalP <- sum(subset(DRINKERS, GROUP == "P")$SCORE)
nP <- nrow(subset(DRINKERS, GROUP=="P"))

# finding CM
n <- nrow(DRINKERS)
CM <- sum(DRINKERS$SCORE)^2/n; CM
SS_total <- sum(DRINKERS$SCORE^2) - CM; SS_total
sst <- totalAR^2/nAR + totalAC^2/nAC + totalA^2/nA + totalP^2/nP - CM
sst
sse = SS_total - sst; sse
Fstat = (sst/(4-1))/(sse/(n-4)); Fstat
1 - pf(Fstat, df1=4-1, df2=n-4)

# same as anova
anova(drinkers.lm)

# also
SSyy(drinkers.lm)
SST(drinkers.lm)
SSE(drinkers.lm)

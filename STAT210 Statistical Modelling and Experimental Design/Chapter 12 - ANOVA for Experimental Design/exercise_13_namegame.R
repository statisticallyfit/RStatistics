setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/NAMEGAME.Rdata")

namegameData <- removeWhitespace(NAMEGAME, colsToFix = "GROUP")
nSimple = nrow(subset(namegameData, GROUP=="1"))
nElaborate = nrow(subset(namegameData, GROUP=="2"))
nPairwise = nrow(subset(namegameData, GROUP=="3"))

namegameData$GROUP = c(rep("simple", nSimple), rep("elaborate", nElaborate),
                       rep("pairwise", nPairwise))
is.factor(namegameData$GROUP)
namegameData$GROUP <- factor(namegameData$GROUP)


# 1 =  simple name game, 2 = elaborate name game, 3 = pairwise intros. 
name.lm <- lm(RECALL ~ GROUP, data=namegameData) # p = 3 treatments
summary(name.lm)
anova(name.lm)
# or can get
SSyy(name.lm)
SST(name.lm)
SSE(name.lm)

anova(name.lm)

# OR
grandmean <- mean(namegameData$RECALL)
meanSimple <- mean(subset(namegameData, GROUP == "simple")$RECALL)
meanElaborate <- mean(subset(namegameData, GROUP == "elaborate")$RECALL)
meanPairwise <- mean(subset(namegameData, GROUP == "pairwise")$RECALL)
nSimple = nrow(subset(namegameData, GROUP=="1"))
nElaborate = nrow(subset(namegameData, GROUP=="2"))
nPairwise = nrow(subset(namegameData, GROUP=="3"))
sb = sum(nSimple*(meanSimple - grandmean)^2 + 
               nElaborate*(meanElaborate - grandmean)^2 + 
               nPairwise*(meanPairwise - grandmean)^2)
sb
SST(name.lm)

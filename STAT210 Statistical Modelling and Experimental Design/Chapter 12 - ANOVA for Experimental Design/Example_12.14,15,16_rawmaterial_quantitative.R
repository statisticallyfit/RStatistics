setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/RAWMATERIAL.Rdata")
options(digits=10, show.signif.stars = FALSE)

# preparing qualitative data
rawCategData <- RAWMATERIAL
rawCategData$RATIO <- factor(rawCategData$RATIO)
rawCategData$SUPPLY <- factor(rawCategData$SUPPLY)

rawCategData$SUPPLY <- relevel(rawCategData$SUPPLY, "21")
levels(rawCategData$SUPPLY)
rawCategData$RATIO <- relevel(rawCategData$RATIO, "2")
levels(rawCategData$RATIO)

# preparing quantitative data
rawQuantData <- RAWMATERIAL


# raw material model with qualitative predictors
raw.categ.lm <- lm(PROFIT ~ RATIO * SUPPLY, data=rawCategData)
summary(raw.categ.lm)
anova(raw.categ.lm)

# raw material model with QUANT predictors
raw.quant.lm <- lm(PROFIT ~ SUPPLY + I(SUPPLY^2) + RATIO + I(RATIO^2) + 
                         RATIO:SUPPLY + SUPPLY:I(RATIO^2) + RATIO:I(SUPPLY^2) + 
                         I(RATIO^2):I(SUPPLY^2), data=rawQuantData)
summary(raw.quant.lm)
anova(raw.quant.lm)

# global F-test
# cate model
raw.categ.null.lm <- lm(PROFIT ~ 1, data=rawCategData)
raw.quant.null.lm <- lm(PROFIT ~ 1, data=rawQuantData)
anova(raw.categ.null.lm, raw.categ.lm)
anova(raw.quant.null.lm, raw.quant.lm)
# OR
NestedFTest(raw.quant.null.lm, raw.quant.lm) # same as for categ

# interaction, and main effects tests for the factorial model of supplyxratio
# (Cateogrical)
anova(raw.categ.lm)



# interaction test
raw.categ.main.lm <- lm(PROFIT ~ SUPPLY + RATIO, data=rawCategData)
raw.quant.main.lm <- lm(PROFIT ~ SUPPLY + I(SUPPLY^2) + RATIO + I(RATIO^2),
                        data=rawQuantData)
anova(raw.categ.main.lm, raw.categ.lm)
anova(raw.quant.main.lm, raw.quant.lm)


# higher order test (for quant model only)
raw.quant.nohigher.lm <- lm(PROFIT ~ SUPPLY * RATIO, data=rawQuantData)
anova(raw.quant.nohigher.lm, raw.quant.lm)
# OR
NestedFTest(raw.quant.nohigher.lm, raw.quant.lm)


# higher order for 3rd, 4th orders
# complete model
raw.quant.lm
# reduced mdoel (testing that 3rd, 4th order terms = 0)
reduced.lm <- lm(PROFIT ~ SUPPLY + I(SUPPLY^2) + RATIO + I(RATIO^2) + 
                       RATIO:SUPPLY, data=rawQuantData)
df = NestedFTest(reduced.lm, raw.quant.lm)
df$RecommendedModel
df$RecommendedName

setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/RAWMATERIAL.Rdata")
options(digits=10, show.signif.stars = FALSE)

RAWMATERIAL$RATIO <- factor(RAWMATERIAL$RATIO)
RAWMATERIAL$SUPPLY <- factor(RAWMATERIAL$SUPPLY)

RAWMATERIAL$SUPPLY <- relevel(RAWMATERIAL$SUPPLY, "21")
levels(RAWMATERIAL$SUPPLY)
RAWMATERIAL$RATIO <- relevel(RAWMATERIAL$RATIO, "2")
levels(RAWMATERIAL$RATIO)

# now with this releveling, the answer summary is the same as textbook. 
raw.lm <- lm(PROFIT ~ SUPPLY + RATIO + SUPPLY:RATIO, data=RAWMATERIAL)
summary(raw.lm)

# contains test of interaction: F = 4.8, p = 0.008, significant interaction
anova(raw.lm)
with(RAWMATERIAL, interaction.plot(x.factor=SUPPLY, trace.factor = RATIO, response=PROFIT))
interactionPlot(data=RAWMATERIAL, xFactor = "SUPPLY", traceFactor = "RATIO", response="PROFIT")

# global f-test
raw.null.lm <- lm(PROFIT ~ 1, data=RAWMATERIAL)
anova(raw.null.lm, raw.lm)

# just main effects model
raw.main.lm <- lm(PROFIT ~ SUPPLY + RATIO, data=RAWMATERIAL)
summary(raw.main.lm)
anova(raw.main.lm)
anova(raw.null.lm, raw.main.lm)

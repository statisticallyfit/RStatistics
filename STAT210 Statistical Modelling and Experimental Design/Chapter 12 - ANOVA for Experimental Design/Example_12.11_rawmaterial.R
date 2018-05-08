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




# doing the anova with formulas
b = length(unique(RAWMATERIAL$SUPPLY)) # num levels of factor B = supply
a = length(unique(RAWMATERIAL$RATIO))
n = nrow(RAWMATERIAL)
r = n / (a*b); r

CM = sum(RAWMATERIAL$PROFIT)^2/n; CM
SS_total = sum(RAWMATERIAL$PROFIT^2) - CM; SS_total
SSyy(raw.lm)
# for A = ratio
A1 = sum(subset(RAWMATERIAL, RATIO == "0.5")$PROFIT)
A2 = sum(subset(RAWMATERIAL, RATIO == "1")$PROFIT)
A3 = sum(subset(RAWMATERIAL, RATIO == "2")$PROFIT)
ss_a = (A1^2 + A2^2 + A3^2)/(b*r) - CM; ss_a
# for B = supply
B1 = sum(subset(RAWMATERIAL, SUPPLY == "15")$PROFIT)
B2 = sum(subset(RAWMATERIAL, SUPPLY == "18")$PROFIT)
B3 = sum(subset(RAWMATERIAL, SUPPLY == "21")$PROFIT)
ss_b = (B1^2 + B2^2 + B3^2)/(a*r) - CM; ss_b
# ss_ab
AB_0.5_15 = sum(subset(RAWMATERIAL, RATIO=="0.5" & SUPPLY=="15")$PROFIT)
AB_0.5_18 = sum(subset(RAWMATERIAL, RATIO=="0.5" & SUPPLY=="18")$PROFIT)
AB_0.5_21 = sum(subset(RAWMATERIAL, RATIO=="0.5" & SUPPLY=="21")$PROFIT)
AB_1_15 = sum(subset(RAWMATERIAL, RATIO=="1" & SUPPLY=="15")$PROFIT)
AB_1_18 = sum(subset(RAWMATERIAL, RATIO=="1" & SUPPLY=="18")$PROFIT)
AB_1_21 = sum(subset(RAWMATERIAL, RATIO=="1" & SUPPLY=="21")$PROFIT)
AB_2_15 = sum(subset(RAWMATERIAL, RATIO=="2" & SUPPLY=="15")$PROFIT)
AB_2_18 = sum(subset(RAWMATERIAL, RATIO=="2" & SUPPLY=="18")$PROFIT)
AB_2_21 = sum(subset(RAWMATERIAL, RATIO=="2" & SUPPLY=="21")$PROFIT)
cs <- c(AB_0.5_15, AB_0.5_18, AB_0.5_21, AB_1_15, AB_1_18, AB_1_21, 
        AB_2_15, AB_2_18, AB_2_21)
ss_ab = (1/r)*(sum(cs^2)) - ss_a - ss_b - CM; ss_ab

# sse
sse = SS_total - ss_a - ss_b - ss_ab; sse

anova(raw.lm)
ss_a # ratio
ss_b # supply
ss_ab
sse

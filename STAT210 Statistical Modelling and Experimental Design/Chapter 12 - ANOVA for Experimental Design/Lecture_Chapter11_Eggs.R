setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 12 - ANOVA for Experimental Design")

eggsData <- data.frame(Pen=c(rep("1", 3), rep("2", 3), rep("3", 3), rep("4", 3)),
                       Daylight=rep(c("O","E","F"), 4),
                       EggsLaid=c(330,372,359, 288,340,337, 295,343,373, 313,341,302),
                       stringsAsFactors = TRUE)
levels(eggsData$Pen)
levels(eggsData$Daylight)
eggsData$Daylight <- relevel(eggsData$Daylight, ref="O")
# O = control (natural daylight), E = extended daylight, F = flash lighting. 


# Incorrect way - no block s- complete randomized. 
eggs.crd.lm <- lm(EggsLaid ~ Daylight, data=eggsData)
anova(eggs.crd.lm)
# No real difference in mean number of eggs for the different daylight conditions. 
# ABOUT RESIDUAL SSES = 4651.8. Below we halved the SSE by including the block. 


# blcoks
eggs.blocks.lm <- lm(EggsLaid ~ Pen + Daylight, data=eggsData)
anova(eggs.blocks.lm)
# Got a significant difference in mean number of eggs laid for different daylights
# across pens. 
# (block may not be significant, but the impact of it on treatment has increased
# the power of the test. )


# Testing blocks and treatment effects
reduced.blocks.lm <- lm(EggsLaid ~ Pen, data=eggsData)
reduced.treats.lm <- lm(EggsLaid ~ Daylight, data=eggsData)

# 1) testing block effects: reduced = treat (all blocks zero)
#NestedFTest(reduced.treats.lm, eggs.blocks.lm)
anova(reduced.treats.lm, eggs.blocks.lm) # same
# 2) testing treatment effects: reduce d= blocks (all treats 0 )
#NestedFTest(reduced.blocks.lm, eggs.blocks.lm)
anova(reduced.blocks.lm, eggs.blocks.lm)

# so these Fs and pvalues are the same as in the block anova test for 
# the complete blocked model:
anova(eggs.blocks.lm)



# Comparing CI's
betaCI(eggs.crd.lm)
betaCI(eggs.blocks.lm)
# lightE = difference in mean response between E and O is same for both
# the randomized abd block designs but stderrors have been reduced in block design
# and confints: in randomized, lightF is not signif but in blocks.lm light F is signif. 


# NOTE: unexplained variability (SSE) is reduced by introducing a blocking effect
# thus increasing power of the test to detect differences due to light treatment. 
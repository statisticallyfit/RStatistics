setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_7_RatsPlants_Contrasts")

options(show.signif.stars = FALSE)

ratData <- read.table("ratWeights.txt", header=TRUE)
is.factor(ratData$Treatment)


# Contrasts: 
# H(i): mu_DOC == mu_WKY
# H(ii): (mu_DOC_CA + MU_WKY_CA)/2 = (mu_DOC + mu_WKY_CA)/2
# H(III): mu_DOC_CA = mu_WKY_CA

CAvNoCa <- C(ratData$Treatment, c(2,-2,2,-2), how.many = 1)
DOCvWKY <- C(ratData$Treatment, c(1,0,-1,0), 1)
DOCCAvWKYCA <- C(ratData$Treatment, c(0,1,0,-1),1)

ratcalcium.lm <- lm(Weight ~ DOCvWKY + CAvNoCa + DOCCAvWKYCA, data=ratData)
betaCI(ratcalcium.lm)
summary(ratcalcium.lm)
getContrastMatrix(ratcalcium.lm)
testContrastsOrthogonal(ratcalcium.lm)

anova(ratcalcium.lm)
# INTERPRET: 
# -- there is significant difference between group mean weights for DOC
# treated rats and untreated WKY rats (p = 0.0000247)
# -- there is significant difference in mean weights for rats that are CA versus
# not Ca treated, so calcium has an effect over all levels of DOC and WKY (p = 0.0055)
# -- there is significant difference in mean weights for CA between treatments since
# p = 0.013. 


getContrastMatrix(ratcalcium.lm)
betaCI(ratcalcium.lm)
# INTERPRET: 
# -- DOC_WKY = DOC mean  y is less than WKY significantly since confint < 0
# -- Ca_noca = CA mean y is greater than NoCa mean y since confint > 0. 
# -- DOCCA_WKYCa = DOCCa mean y is less than WKYCA mean y. 
      


# Testing - interpretation is the same only signs would change
DOCvWKY <- C(ratData$Treatment, c(-1,0,1,0), 1)
ratcalcium2.lm <- lm(Weight ~ DOCvWKY + CAvNoCa + DOCCAvWKYCA, data=ratData)
getContrastMatrix(ratcalcium2.lm)

betaCI(ratcalcium2.lm)

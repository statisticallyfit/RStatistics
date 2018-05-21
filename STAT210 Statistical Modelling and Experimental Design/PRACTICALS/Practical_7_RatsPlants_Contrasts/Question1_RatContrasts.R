setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_7_RatsContrasts")

options(show.signif.stars = FALSE)

ratData <- read.table("ratWeights.txt", header=TRUE)
is.factor(ratData$Treatment)


# Contrasts: 
# H(i): mu_DOC == mu_WKY
# H(ii): (mu_DOC_CA + MU_WKY_CA)/2 = (mu_DOC + mu_WKY_CA)/2
# H(III): mu_DOC_CA = mu_WKY_CA

CAvNoCa <- C(ratData$Treatment, c(2,2,-2,-2), how.many = 1)
DOCvWKY <- C(ratData$Treatment, c(1,-1,0,0), 1)
DOCCAvWKYCA <- C(ratData$Treatment, c(0,0,1,-1),1)

ratcalcium.lm <- lm(Weight ~ DOCvWKY + CAvNoCa + DOCCAvWKYCA, data=ratData)
getContrastMatrix(ratcalcium.lm)
testContrastsOrthogonal(ratcalcium.lm)

anova(ratcalcium.lm)
# INTERPRET: 
# -- there is NO significant difference between group mean weights for DOC
# treated rats and untreated WKY rats (F = 0.92, p = 0.339)
# -- there is significant difference in mean weights for rats that are CA versus
# not Ca treated, so calcium has an effect over all levels of DOC and WKY (p = 3.2e-6)
# -- there is significant difference in mean weights for CA between treatments since
# p = 0.005. 



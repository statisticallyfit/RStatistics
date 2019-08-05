
# STEP 2: remove the 3-way interaction term
mice.alltwoway.binom.glm <- glm(TumourCount / Total ~ (Strain + Gender + Exposure)^2, 
                                data=miceData.binom, family=binomial, weights=Total)

anova(mice.alltwoway.binom.glm, test="Chisq")
# INTERPRET: 
# all the two way terms have p-values > 0.05 for their deviance statistics, so they 
# are not significant predictors of number of tumor cases. 


mice.2twoway.binom.glm <- glm(TumourCount / Total ~ Strain+Gender+Exposure+Strain:Gender + Strain:Exposure, 
                              data=miceData.binom, family=binomial, weights=Total)
anova(mice.2twoway.binom.glm, test='Chisq')


mice.1twoway.binom.glm <- glm(TumourCount / Total ~ Strain+Gender+Exposure+Strain:Gender , 
                              data=miceData.binom, family=binomial, weights=Total)
anova(mice.1twoway.binom.glm, test='Chisq')


mice.indep.binom.glm <- glm(TumourCount / Total ~ Strain + Gender + Exposure , 
                            data=miceData.binom, family=binomial, weights=Total)
anova(mice.indep.binom.glm, test='Chisq')

# The independence model fits best. 

anova(mice.indep.binom.glm, mice.saturated.binom.glm, test="Chisq")

anova(mice.saturated.binom.glm)


# ================================================

# Gender  + Exposure 
indep.ge <- glm(tumourCount ~ Gender + Exposure , data=miceData.binom, family=binomial)
indep.ges <- glm(tumourCount ~ Gender + Exposure + Strain , data=miceData.binom, family=binomial)
anova(indep.ge, test="Chisq")
anova(indep.ges, test="Chisq")
#
anova(indep.ges, test='Chisq') #same p-values of first three terms, as long as 'ges' is order same
anova(sat.ges, test="Chisq")


# =====================
sat.sge = mice.saturated.binom.glm                     ###
#sat.seg = glm(tumourCount ~ Strain * Exposure * Gender, 
#              data=miceData.binom, family=binomial)
#sat.ges = glm(tumourCount ~  Gender * Exposure * Strain, 
#              data=miceData.binom, family=binomial)
sat.gse = glm(tumourCount ~ Gender * Strain * Exposure,  ###
              data=miceData.binom, family=binomial)
#sat.egs = glm(tumourCount ~ Exposure * Gender * Strain, 
#              data=miceData.binom, family=binomial)
#sat.esg = glm(tumourCount ~ Exposure * Strain * Gender, 
#    data=miceData.binom, family=binomial)

anova(sat.sge, test="Chisq")
anova(sat.seg, test="Chisq")
anova(sat.ges, test="Chisq") # possible two-term indep model: G + E
anova(sat.gse, test="Chisq")
anova(sat.egs, test="Chisq") # possible two-term indep model: E + G
anova(sat.esg, test="Chisq")

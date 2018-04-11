setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_4_MapleSamara/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')

library(ggplot2)
library(lattice)
options(digits=10, show.signif.stars = F)

samaraData <- read.table("samara.txt", header=TRUE)
samaraData$Tree <- factor(samaraData$Tree, labels = c("T1", "T2", "T3"))

xyp <- xyplot(Velocity ~ Load | Tree, data=samaraData, 
              layout=c(3,1),
              panel=function(x,y){
                    panel.xyplot(x, y)
                    panel.lmline(x, y)
              })
xyp

# Research question to ask: for each level of tree, as load increases,
# does Velocity of samara increase differently for each level of tree? 
# We can see the intercepts are different for lower loads but similar for higher
# loads and there is a sharper increase in velocity for tree level 2 as 
# load increases versus the other tree levels 1, and 3. 

# More scatter in velocity for levels 1 and 3 of Tree, and least squares line
# for tree 3 is heavily influenced by Load = 0.155 and Load = 0.187. 






# PART b, i) FItting three models, interaction, main effects,a nd simple

# MODEL 1: INTERACTION
samara.interact.lm <- lm(Velocity ~ Load * Tree, data=samaraData)
anova(samara.interact.lm)
# the interaction term is marginally significant


# MODEL 2: MAIN EFFECTS
samara.main.lm <- lm(Velocity ~ Load + Tree, data=samaraData)
anova(samara.main.lm)
# given we fitted Load, tree is not significant. 


# MODEL 3: simple linear regression with just Load
samara.simple.lm <- lm(Velocity ~ Load, data=samaraData)
anova(samara.simple.lm)
#summary(samara.simple.lm) # same last p-values




# COMPARE THE THREE MODELS
anova(samara.interact.lm, samara.main.lm, samara.simple.lm)

# INTERPRET: 
# 2nd row - pvalue=0.0501 marginally significant, tests between Model Interaction
# and Model Main. We see the differing term (Load:Tree) is not significant because
# of 0.05 p-value. 

# 3rd row - pvalue=0.38. The only differing term between Model Main and Model Simple
# is the tree term and p=0.38 so Tree is not significant. 

# CONCLUDE: only the Simple Load model is left so choose that one. 
# But if we chose alpha sig level = 0.10 then we would go with the interaction
# model. 





# PART b, ii) fit maximal model, forwards stepwise regression to test whether
# interaction should be retained

formLower <- formula( ~ 1)
formUpper <- formula( ~ Load + Tree + Load:Tree, data=samaraData)

start.model <- lm(Velocity ~ 1, data=samaraData)

step.forward.model <- step(start.model, direction="forward", 
                           scope=list(lower=formLower, upper=formUpper))

# We started with minimal model and AIC for minimal is -120.45. Adding
# Load and Tree lowers the AIC respectively by -174.27 and -141.39. Load
# gives the lowest AIC so choose Load as the predictor. 

# Now in final step, adding tree reduces AIC from -174.27 to -172.17 so 
# do not choose Tree. Just remain with the Load model. 

summary(step.forward.model)

# INTERPRET COEFS: Load coef is significant. 





# PART b, iii) fit backwards stepwise
formLower
formUpper

start.model <- lm(Velocity ~ Load + Tree + Load:Tree, data=samaraData)

step.back.model <- step(start.model, direction = "backward",
                        scope=list(lower=formLower, upper=formUpper))

# AIC for model with interaction term is -175.4 and removing the
# interaction term Load:Tree would increase AIC to -172.17
# So choose to keep it. final model contains interaction term. 

summary(step.back.model)
anova(step.back.model)
betas <- betaCI(step.back.model)
betas

# INTERPRET: 
# * Load:Tree2 confint is (0.67, 6.8) so the increase in velocity
# for a unity increase in load is significantly higher for Tree 2
# than for tree 1. The Tree 2 velocity mean is greater significantly
# then the Tree 1 velocity mean level because the confint is above 0. 

# Intercept difference for Tree 2 vs Tree 1 is significantly
# different from 0. Confint is all below 0, means that mean
# velocity for Tree 2 is significantly lower than for Tree 1 (intercept
# on graph). confint = (-1.527, -0.154)

# * Load:Tree3 confint is (-3.85, 5.49) so there is no significant
# difference in velocity RATE (or slope) between Tree 3 and tree 1 since this 
# confint  includes 0. (diff of slopes not significant)

# Intercept difference for Tree 3 vs Tree 1: not significant since confint
# (-1.209, 0.612)  contains 0. We see on plot that intercept for Tree 1 is at 
# around 1  and for Tree 3 it is around 0.8, so this is confirmed. 



# part e) regression lines for each tree

# TREE 1 (x2, x3 = 0)
cat(betas[1,1], " + ", betas[2,1], " * Load", sep="")

# TREE 2 (x2 = 1, x3 = 0)
cat(betas[1,1] + betas[3,1], "+", betas[2,1] + betas[5,1], "* Load")


# TREE 3 (x2 = 0, x3 = 1)
cat(betas[1,1] + betas[4,1], "+", betas[2,1] + betas[6,1], "* Load")







# DIAGNOSTIC PLOTS for interaction model
residualFittedPlot(samara.interact.lm) # good scatter, constant variance
normalQQPlot(samara.interact.lm) # good straight line normality
shapiro.test(samara.interact.lm$residuals) # non-sig p-value so no evidence
# of strong deviation from noramlity. 

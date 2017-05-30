
### see how to calc chisquare for logistic regression in Hosmer pg 176 pdf
# download data from: 
# https://github.com/lbraglia/aplore3/tree/master/data
# GLOW500
load("glow500.rda")
head(glow500)
names(glow500)
library(plyr)
# combining the levels Same and Less to be the same, leaving Greater alone
glow500$raterisk <- mapvalues(glow500$raterisk, from=c("Same","Less"), 
                              to=c("Same/Less", "Same/Less"))
glow.model <- glm(fracture ~ age + height + priorfrac + momfrac + armassist +
                        raterisk + age:priorfrac + momfrac:armassist,
                  data=glow500,
                  family=binomial(logit))
coef(glow.model)

anova(glow.model)

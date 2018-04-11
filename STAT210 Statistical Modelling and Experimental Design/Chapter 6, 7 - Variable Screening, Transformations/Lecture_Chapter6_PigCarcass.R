setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 6, 7 - Variable Screening, Transformations/lecturedata/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R')
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Rfunctions.R')


options(digits=10, show.signif.stars = F)


pigData <- read.table("carc.txt", header=TRUE)

# exploratory plots
pairsQuantPlot(pigData, c(2:7,1), size=5)

pairs(pigData,lower.panel=panel.smooth, upper.panel=panel.cor)
# INTERPRET: fdP2 and fd34 have very strong positive correlation (r=0.93) - muscle depths
# and there is reasonably strong pos cor between fdH1 and fdH2 (r=0.66)  - muscle weight
# so which subset will we need? - one each of muscle deptha nd muscle weight?
# There is also negative cor with response LMPC  
# and each of fdP2, fd34 (r=-0.72, r=-0.74)


# Stepwise
formL <- formula(~ 1)
formU <- formula(~ Wt + fdP2 + fd34 + fdH1 + fdH2 + mdP2 + md34)
start.model <- lm(LMpc ~ 1, data=pigData)

step.forward.model <- step(start.model, direction = "forward",
                           scope=list(lower=formL, upper=formU))
# fdH2 gives same AIC 114 - for parsimony, don't include this extra term
# but if you want, ou can. Can even swap with fd34. But there may be 
# practical (e.g. cost) reasons why you might want to include one over the other. 

summary(step.forward.model)


betaCI(step.forward.model)
# All estimates are significant. Terms chosen with AIC may end up with NON
# significant p-values. There may be inconsistencies. Do not mix the two
# approaches, like swapping one with the other or using one to justify
# the other. 


# Backward selection
start.model <- lm(LMpc ~ Wt + fdP2 + fd34 + fdH1 + fdH2 + mdP2 + md34,data=pigData)
step.back.model <- step(start.model)

# results will not always be the same between forward and backward selection. 

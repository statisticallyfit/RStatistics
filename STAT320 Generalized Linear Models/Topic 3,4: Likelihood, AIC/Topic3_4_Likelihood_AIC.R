setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/Topic 1: The Statistical Model")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')



library(ggplot2)


# PLOTTING the likelihood function for the example

likPi <- 0
piValues <- seq(0, 1, 0.001)
for(i in 0:1000) {
      pi <- i / 1000
      likPiValue <- choose(10, 3) * pi^3 * (1 - pi)^7
      likPi <- rbind(likPi, likPiValue)
}

likPiList <- likPi[2 : length(likPi), ]

#plot(likPiList ~ piValues, type="l", main=expression("L"(pi) ~ "vs" ~ pi), 
#     ylab=expression("Likelihood, L"(pi)), xlab=expression(pi))


df <- data.frame(PI=piValues, LIKPI = likPiList)
ggplot(data=df, aes(x=PI, y=LIKPI)) + geom_line() + ggtitle("Likelihood vs PI values") + 
      geom_vline(xintercept=0.3, linetype="dashed", color="red")




# EXAMPLE 4.2.2 Fuel Data Likelihood + AIC ---------------------------------------------

## GOAL ##
# Fit a model to the data

options(show.signif.stars = FALSE)
library(ggplot2)
library(GGally)
library(AICcmodavg)

## EXAMPLE 1.1 #########################################################################
fuelData <- read.table("fuel.txt", header=TRUE)


# Step 1 - exploratory data analysis 

# Observe: Mileage is closely related to both Weight and Disp
# Weight and Disp are also highly correlated
ggpairs(fuelData, columns=c(2,3,4))

# Model selection based on AIC weights

candidateModels <- list()
candidateModels[[1]] <- lm(Mileage ~ Weight + Disp, data=fuelData)
candidateModels[[2]] <- lm(Mileage ~ Weight, data=fuelData)
candidateModels[[3]] <- lm(Mileage ~ Disp, data=fuelData)

modelNames <- c("modelWD", "modelW", "modelD")

#table of models with AICc, weighted, LL
# sorted by the delta AIC values, where deltaAICc_i = AICc_i - AICc_min
aics <- aictab(cand.set = candidateModels, modnames = modelNames, sort=TRUE); aics

# The model with lowest AICc is the Weight model
# Aikake weight = probabiliy of this model = is 0.75
# Means the probability of that model being the best approximating model from amongs the
# models in the candidate set, given the data, is 0.75

# Evidence ratio for each model: 
deltas <- aics$Delta_AICc

# So model Weight is 100% more likely than model Weight
ER_modelW <- exp(-(1/2)*deltas[1]) / exp(-(1/2) * deltas[1]); ER_modelW

# model weight is 3.019 times more likely than model Weight (evidence for model Weight
# is 3 imes stronger than for the second model, model Weight + Disp)
ER_modelWD <- exp(-(1/2)*deltas[1]) / exp(-(1/2) * deltas[2]); ER_modelWD

# Evidence for model Weight is 99843806 times stronger than for model Disp
ER_modelD <- exp(-(1/2)*deltas[1]) / exp(-(1/2) * deltas[3]); ER_modelD


# NOTE: models with deltas between 2-7 should not be ignored, but any higher than > 14 then
# the model is implausible, so here the DIsp model is implausible. 

# Verifying the Aikake weights: 
# Model weight: 
w_weight <- exp(-(1/2)*deltas[1]) / sum(exp(-1/2*deltas)); w_weight
d_weight <- exp(-(1/2)*deltas[3]) / sum(exp(-(1/2)*deltas)); d_weight
wd_weight <- exp(-(1/2)*deltas[2]) / sum(exp(-1/2*deltas)); wd_weight

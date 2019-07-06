setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/Topic 1: The Statistical Model")
source('/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')

## GOAL ##
# Fit a model to the data

options(show.signif.stars = FALSE)
library(ggplot2)
library(GGally)
# library(dae) # interaction plots ? 
library(effects) # effects plot
library(ggeffects)
library(lattice)


## EXAMPLE 1.1 #########################################################################
fuelData <- read.table("fuel.txt", header=TRUE)


# Step 1 - exploratory data analysis 

# Observe: Mileage is closely related to both Weight and Disp
# Weight and Disp are also highly correlated
ggpairs(fuelData, columns=c(2,3,4))

# Step 2 - propose and fit a model (normal errors, linear, y = mileage, x = weight)
fuel.weight.lm <- lm(Mileage ~ Weight, data=fuelData)
summary(fuel.weight.lm)


# Step 3 - check the systematic part of the model

# Comment: the linear fit appears rough, the observatoins at the ends are
# curved above the line and in the middle the observations are below the line. 
modelPlot(fuel.weight.lm)


# Step 4 - check the random part

# Check the distribution of residuals: density plot, qq plot ...resids vs fitted values
residualFitPlot(fuel.weight.lm) 

# the line departs a bit from the qq theoretical distribution of errors
autoplot(fuel.weight.lm)

# distributions not quite the same
residualDensityPlot(fuel.weight.lm)

# Using QQ plot alone, normality assumption seends sound but residual scatter plot
# shows non-random scatter around 0 so the systematic effect (linear) is not reliably
# account for mileage vs weight. 



# PLOT RESIDUALS VS EXTRA EXPLANATORY VARIABLES

# We are going to plot the previous residuals vs displacement to see if any
# further information is contained in the residuals. 

ggplot(data=fuelData, aes(x=Disp, y=fuel.weight.lm$residuals)) + 
      geom_point(shape=19) + 
      ggtitle("Old Model Residuals vs Displacement")
# Comment: seems to be no pattern to this plot, so no useful information is leftover
# in the residuals from fitting Weight. 


# 1.2 AOV #########################################################################
anova(fuel.weight.lm)

fuel.weightdisp.lm <- lm(Mileage ~ Weight + Disp, data=fuelData)
anova(fuel.weightdisp.lm)

# See that Displacement is not an important predictor after fitting Weight

fuel.disp.lm <- lm(Mileage ~ Disp, data=fuelData)

# Compare which predictor is more useful: Disp or weight? 
anova(fuel.disp.lm)
anova(fuel.weight.lm)
# MS for displacement is lower than MS for weight (973.75 vs 650) so weight is
# still the better predictor. 



# Example 1.3 - Salary and Gender ##############################################

# Gender: 1 = male, 2 = female
# YrsEm = years employed
salaryData <- read.table("salary.txt", header=TRUE)
salaryData$Gender <- factor(salaryData$Gender, labels=c("M", "F"))
salaryData.nocontr <- salaryData

# Define contrasts
contrasts(salaryData$Gender) <- c(0.5, -0.5)
G.names <- c("M", "F")

# Set up lattice graphics, no color, double line width
line <- trellis.par.get("plot.line")
line$lwd=2
trellis.par.set("plot.line", line)

# Exploratory xyplot
xyp.eda1 <- xyplot(Salary ~ YrsEm|Gender, data=salaryData, 
                   panel=function(x,y) { 
                         panel.xyplot(x, y); 
                         panel.loess(x, y)
                         })
print(xyp.eda1)


# Plot of salary vs yrsEm for each gender
interactionPlot(data=salaryData, xFactor="YrsEm", traceFactor="Gender", response="Salary")

# Fit the interaction model, including the design matrix
salary.interact.lm <- lm(Salary ~ Gender*YrsEm, data=salaryData, x = TRUE)
salary.interact.nocontr.lm <- lm(Salary ~ Gender*YrsEm, data=salaryData.nocontr, x=TRUE)

# ANOVA summary (for model with contrasts we set )
summary(salary.interact.lm)
anova(salary.interact.lm)
aov(salary.interact.lm)

# Anova summary for model with auto-generated contrasts
summary(salary.interact.nocontr.lm)

# Compare contrasts
getContrastMatrix(salary.interact.lm)
salary.interact.lm$x

getContrastMatrix(salary.interact.nocontr.lm)
salary.interact.nocontr.lm$x

v1 = salary.interact.lm$x[,2]
v2 = salary.interact.nocontr.lm$x[,2]
cbind(v1, v2)


# Effects plot
salary.eff <- allEffects(salary.interact.lm)
print(salary.eff)
plot(salary.eff)
# or do with ggplotting --- HELP does is warning
#preds <- ggpredict(salary.interact.lm, full.data = TRUE, terms=c("YrsEm", "Gender"))
#preds
#plot(preds) #+ theme_gray()


# Diagnostics: 
## from QQ plot: non straight so nonnormal residuals
## resids vs fitted: curvature or fan shape so nonconstant variance. 
autoplot(salary.interact.lm)

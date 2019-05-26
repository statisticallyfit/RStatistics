setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 9 - Logistic Regression")

library(ggplot2)


options(show.signif.stars = FALSE)

# source: 
# https://www.fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-i/

leafData <- read.table("darlington.txt", header=TRUE)
leafData <- setNames(leafData, nm=c("LeafHeight", "Visited"))
leafDataFactor <- leafData
leafDataFactor$Visited <- factor(leafData$Visited)

head(leafData)


# Plotting density estimates of distributiosn of leaf heights for 
# visited and unvisited leaves. 
ggplot(leafDataFactor, aes(x=LeafHeight, colour=Visited)) + 
      geom_line(stat="density", size=2) + 
      xlab("Leaf height (cm)") + ylab("Density")

ggplot(leafDataFactor, aes(x=Visited, y=LeafHeight, colour=Visited)) + 
      geom_boxplot(size=1, outlier.colour = "black", outlier.size = 6)

# for non visited leaves, the height is smaller. Is this significant? 
summary(lm(LeafHeight ~ Visited, data=leafDataFactor)) # yes there is  a significant diff.

ggplot(data=wineData, aes(x=fixed.acidity, colour=quality)) + 
      geom_line(sta="density", size=2)

# BUT NOW LOGISTIC (Visited is the response)
# Modeling probability of leaf visitation as function of height. 
leaf.glm <- glm(Visited ~ LeafHeight, data=leafData, family=binomial)
summary(leaf.glm)
anova(leaf.glm, test="Chisq")

DevianceTest(leaf.glm)
# There is a significant deviance between the null model and the height model
# so the new model explains most of the variation in visitation. 

ResidualDevianceTest(leaf.glm)
# model is statistically useful  since residual deviance is small. 


# for a unit increase in leaf height, the odds of visitation 
# increase by 12.23 %. 
exp(leaf.glm$coefficients[2]) - 1


# CONFIDENCE INTERVAL PLOTTING
from = min(leafData$LeafHeight)
to = max(leafData$LeafHeight)
xs <- data.frame(LeafHeight=seq(from=from,to=to, len=nrow(leafData)))

# the levels are by default 95% for the CIs
CI <- data.frame(predict(leaf.glm, newdata=xs, type="response", se.fit=TRUE))
pred.df <- data.frame(LeafHeight=xs$LeafHeight, fit=CI$fit, 
                      lwr=CI$fit - 2*CI$se.fit, upr=CI$fit + 2*CI$se.fit)

# Plotting confidence bands 
p.data = ggplot(leafData, aes(x=LeafHeight, y=Visited)) + 
      geom_point(shape=19, size=3) 

p.fits = p.data + 
      geom_line(data=pred.df, aes(y=fit, colour="a", linetype="a"),size=1) +
      geom_line(data=pred.df, aes(y=lwr, colour="b", linetype="b"),size=1) + 
      geom_line(data=pred.df, aes(y=upr, colour="b", linetype="b"),size=1) 

p.plot <- p.fits + 
      ggtitle("Predicted and Observed Values of Visited vs LeafHeight 
              and 95% Confidence Bands") +
      scale_colour_manual(name="Legend", values=c("a"="red", "b"="dodgerblue"),
                          labels=c("Fitted Line", "95%\nConfidence\nBands")) +
      scale_linetype_manual(name="Legend", values=c("a"="solid", "b"="dashed"),
                            labels=c("Fitted Line", "95%\nConfidence\nBands"))

p.plot 



# OR ANOTHER WAY  --- (with linkinv function in predict())
# Why are the previous confidence bands going over the dots? (using response preds)
plotConfidenceBands.glm(leaf.glm)

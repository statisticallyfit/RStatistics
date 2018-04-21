setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 9 - Logistic Regression/")

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


# confinds
# the title must be same as x-var name
pred.df <- with(leafData, 
                data.frame(LeafHeight= seq(min(LeafHeight), max(LeafHeight),
                                           length=100)))
temp <- predict(leaf.glm, newdata=pred.df, type="response", se.fit=TRUE)
pred.df <- cbind(pred.df, fit=temp$fit, lwr=temp$fit - 1.96*temp$se.fit,
                 upr=temp$fit + 1.96*temp$se.fit)

p.data <- ggplot(leafData, aes(x=LeafHeight, y=Visited)) + 
      geom_point(shape=19, size=3)
p.data
p.fit <- p.data + 
      geom_line(data=pred.df, aes(y=fit), colour="blue", size=1) + 
      geom_line(data=pred.df, aes(y=lwr), colour="red", linetype="dashed",size=1) + 
      geom_line(data=pred.df, aes(y=upr), colour="red", linetype="dashed", size=1) + 
      ylab("Probability of Visitation of Leaves")
p.fit

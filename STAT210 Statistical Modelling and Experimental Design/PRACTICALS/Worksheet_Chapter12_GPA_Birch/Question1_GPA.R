setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Worksheet_Chapter12_GPA")

options(show.signif.stars = FALSE)

gpaData <- read.table("GPA3.txt", header=TRUE)


gpa.lm <- lm(GPA ~ CLASS, data=gpaData)
anova(gpa.lm)
# There is significant difference in mean GPAs among the three classes. 

#Anova: there is a significant difference between the mean GPAs
#across the three class levels (p = 0.025)

betaCI(gpa.lm)
#Estimates: 
# * The mean GPA is significantly greater than 0 for lower class
# (since confint does not contain 0 and is above 0)
# * The difference in mean GPA for middle and lower class is 
# significantly greater than 0. Est = 0.73.
# * No significant difference in mean GPA for high and low class
#since confint contains 0. 


# Relevel
gpaData$CLASS <- relevel(gpaData$CLASS, ref="Middle")
gpa.mid.lm <- lm(GPA ~ CLASS, data=gpaData)
betaCI(gpa.mid.lm)
# Releveling: 
# * the mean GPA for upper class is significantly lower than mean GPA for 
# middle class since B3 = -0.706 < 0  and the confint is below 0. 
# * (IN FIRST TABLE): the mean GPA for lower class is significantly lower than for middle class 
# since confint is below 0 (shown in  previous output since the middle - lower 
# confint is above 0. Same confint, just the negative of the previous one. 



# ----------------testing means estimates versus tapply -------------

cof <- summary(gpa.lm)$coef
mu_Lower = cof[1,1]; mu_Lower
mu_Middle = mu_Lower + cof[2,1]; mu_Middle
mu_Upper = mu_Lower + cof[3,1]; mu_Upper

with(gpaData, tapply(GPA, INDEX=list(CLASS), mean))
# They match!
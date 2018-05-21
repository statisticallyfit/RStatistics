setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Worksheet_Chapter12_GPA")

options(show.signif.stars = FALSE)

birchData <- read.table("birch.txt", header=TRUE)


with(birchData, 
     interaction.plot(response=ATP, x.factor=Species, trace.factor=Treat))

# the 2x2 factorial design
birch.lm <- lm(ATP ~ Species * Treat, data=birchData)


anova(birch.lm)
# There is a significant interaction between tree species and treatment. 
# So the difference in mean ATP for the two tree levels (River and Euro) depends 
# upon the 2 levels of treatment (control and flood). 

summary(birch.lm)
# No need to interpret main effects since then we would be averaging over the
# other predictor. 
# And,
# B3 = (mu_Riv/Flood - mu_Eur/Flood) - (mu_Riv/Control - mu_Eur/Control) 
# So  significant difference in mean ATP between River and Euro tree species 
# between the Flood and Control groups. 



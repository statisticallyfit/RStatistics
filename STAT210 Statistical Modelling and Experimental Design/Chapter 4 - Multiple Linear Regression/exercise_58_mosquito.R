setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/INTERPRET.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PLOTTING.R', echo=FALSE)


library(ggplot2)
load("data/Exercises and Examples/REPELLENT.Rdata")
load("data/Exercises and Examples/EXECSAL.Rdata")



types <- c()
for(i in 1:nrow(REPELLENT)){
      if(REPELLENT$TYPE[i] == "Lotion/Cream"){
            types = c(types, 1)
      } else {
            types = c(types, 0)
      }
}

repellentData <- data.frame(COSTPERUSE=REPELLENT$COST, MAXHOURS=REPELLENT$HOURS,TYPE=REPELLENT$TYPE)
head(repellentData)
levels(repellentData$TYPE) <- c('0', '1')
repellentData


# Fitting the model with y1 = cost per use
repellent.dummy.lm <- lm(COSTPERUSE ~ TYPE, data=repellentData)
summary(repellent.dummy.lm)
lm(COST ~ TYPE, data=REPELLENT) # (same thing)

# Scatterplot
g <- ggplot(REPELLENT, aes(x = TYPE, y = COST))
g + geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Mosquito Repellent Type", y="Cost per use ($)") 



# Fitting model with y2 = max hours protection
repellent.dummy.lm2 <- lm(HOURS ~ TYPE, data=REPELLENT)
summary(repellent.dummy.lm2)

# Scatterplot
g <- ggplot(REPELLENT, aes(x = TYPE, y = HOURS))
g + geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Mosquito Repellent Type", y="Maximum Protection (hours))") 

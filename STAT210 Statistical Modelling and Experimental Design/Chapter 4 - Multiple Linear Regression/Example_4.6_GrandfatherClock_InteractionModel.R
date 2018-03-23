setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/FORMULAS.R', echo=FALSE)
source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/INTERPRET.R', echo=FALSE)

load("data/Exercises and Examples/GFCLOCKS.Rdata")

# assume interaction term: age * price - rate of increase of price with age 
# is driven upward with number of bidders. 
clock.interact.lm <- lm(PRICE ~ AGE + NUMBIDS + AGE_BID, data=GFCLOCKS)
summary(clock.interact.lm)

# same thing, just include the *
summary(lm(PRICE ~ AGE*NUMBIDS, data=GFCLOCKS))

ggplot(GFCLOCKS, aes(x=AGE, y=PRICE, group = factor(NUMBIDS), 
                     color=factor(NUMBIDS))) + 
      geom_point() + 
      geom_smooth(method="lm", se=FALSE) + 
      ggtitle("Interaction plot of Age With Price For Different
              Levels of Bidders")

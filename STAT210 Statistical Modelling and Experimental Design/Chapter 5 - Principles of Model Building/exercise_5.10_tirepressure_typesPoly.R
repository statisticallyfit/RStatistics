setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/TIRES2.Rdata")
library(ggplot2)


# Plotting y = heatrate against the other independent variables and guessing
# at the model needed.

# Needs upside down quadratic, B2 < 0
g1 <- ggplot(TIRES2, aes(x = X_PSI, y = Y_THOUS)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Pressure (pounds per square inch)", y="Mileage (thousands of miels)")
g1 #

# Straight line for early data
g2 <- ggplot(TIRES2[1:5,], aes(x=X_PSI, y=Y_THOUS)) +
      geom_point(shape=19, color="dodgerblue", size=3) +
      labs(x="Pressure (pounds per square inch)", y="Mileage (thousands of miels)")
g2 #

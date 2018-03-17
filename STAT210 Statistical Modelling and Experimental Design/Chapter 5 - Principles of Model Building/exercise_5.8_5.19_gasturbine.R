setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/GASTURBINE.Rdata")
library(ggplot2)


# Plotting y = heatrate against the other independent variables and guessing
# at the model needed.
g1 <- ggplot(GASTURBINE, aes(x = ENGINE, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Engine", y="Heat Rate")
g1 #

g2 <- ggplot(GASTURBINE, aes(x = SHAFTS, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Shafts", y="Heat Rate")
g2 #

# Quadratic (b2 < 0)
g3 <- ggplot(GASTURBINE, aes(x = RPM, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="RPM", y="Heat Rate")
g3 #

# Quadratic (B2 > 0, and high B1)
g4 <- ggplot(GASTURBINE, aes(x = CPRATIO, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Cycle Pressure Ratio", y="Heat Rate")
g4 #

# Quadratic (B2 > 0, and high B1)
g5 <- ggplot(GASTURBINE, aes(x = INLETTEMP, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Inlet Temperature", y="Heat Rate")
g5 #

g6 <- ggplot(GASTURBINE, aes(x = EXHTEMP, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Exhaust Temperature", y="Heat Rate")
g6 #

g7 <- ggplot(GASTURBINE, aes(x = AIRFLOW, y = HEATRATE)) + 
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Airflow", y="Heat Rate")
g7

g8 <- ggplot(GASTURBINE, aes(x = POWER, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="Power", y="Heat Rate")
g8

# Almost exact straight line curve (straight line good for this range)
g9 <- ggplot(GASTURBINE, aes(x = LHV, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="LHV", y="Heat Rate")
g9 #

# Quadratic (B2 > 0, high B1)
g10 <- ggplot(GASTURBINE, aes(x = ISOWORK, y = HEATRATE)) +
      geom_point(shape=19, color="dodgerblue", size=3) + 
      labs(x="ISOWORK", y="Heat Rate")
g10 #



# Fitting complete second order model
attach(GASTURBINE)
gas.lm <- lm(HEATRATE ~ RPM + CPRATIO + RPM*CPRATIO + I(RPM^2) + I(CPRATIO^2),
             data=GASTURBINE)
summary(gas.lm)$coef

summary(gas.lm)
FTest(gas.lm)



# Graphing prediction equation when RPM = 5000 rpm
ys <- function(x2) {
      return((15582.54 + 0.07823*5000 - 0.00000018*5000^2) + 
      (-523.134 + 0.00445*5000)*x2 + 8.84*x2^2)
}
allYS <- ys(x2=GASTURBINE$CPRATIO)
g <- ggplot(GASTURBINE, aes(x = CPRATIO, y = allYS)) + 
      geom_point(shape=19, color="magenta", size=3)
g #

# Graphing prediction equation when RPM = 15000 rpm
ys <- function(x2) {
      return((15582.54 + 0.07823*15000 - 0.00000018*15000^2) + 
                   (-523.134 + 0.00445*15000)*x2 + 8.84*x2^2)
}
allYS <- ys(x2=GASTURBINE$CPRATIO)
g1 <- ggplot(GASTURBINE, aes(x = CPRATIO, y = allYS)) + 
      geom_point(shape=19, color="magenta", size=3)
g1 #

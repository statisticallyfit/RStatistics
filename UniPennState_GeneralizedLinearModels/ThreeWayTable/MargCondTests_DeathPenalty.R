source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')
#c(19,132,11,52,0,9,6,97)
death <- array(c(19,132,11,52,0,9,6,97), dim=c(2,2,2))
death 
dimnames(death) <- list(DeathPenalty=c("yes","no"),
                        DefendantRace=c("white","black"),
                        VictimRace=c("white","black"))
death 

## create flat contingency table
ftable(death, row.vars=c(2,3), col.vars=1) # same as below
deathTable <- ftable(death, row.vars=c("DefendantRace", "VictimRace"), col.vars="DeathPenalty")
deathTable



############################### Test of mutual independence ##############################
penalty <- margin.table(death, 1); penalty
defend <- margin.table(death, 2); defend
victim <- margin.table(death, 3); victim

### expected values under mutual independence model
# ?"%o%"
# outer product of the arrays
deathExp <- (defend %o% victim %o% penalty) / sum(death)^2; deathExp

# Testing for difference in saturated model vs independence model. 
ChiSquareIndependence(death)





##################### Test of mutual independence via marginal tables ####################
### MARGINAL TABLES
# let A = defendant, B = victim, C = penalty
AB <- margin.table(death, c(2,3)); AB # defend-victim
AC <- margin.table(death, c(1,2)); AC # penalty-defend
BC <- margin.table(death, c(1,3)); BC # penalty-victim

### Chi-square tests for marginal tables
# defend-victim
ChiSquareIndependence(AB) # dependent (reject NULL)
# penalty-defend
ChiSquareIndependence(AC, correct = TRUE) # *marginally* independent
# penalty-victim
ChiSquareIndependence(BC) # dependent (reject NULL)


### Compute odds ratios for the marginal tables
library(vcd)
assocstats(death)
assocstats(AC)
assocstats(AB)
assocstats(BC)

AB 
oddsRatio(AB)

AC
oddsRatio(AC)

BC
oddsRatio(BC)

### Test of conditional independence (Cochran-Mantel-Haenszel for 3-way)
mantelhaen.test(death)
mantelhaen.test(death, correct = FALSE)


### Test of conditional independence via oddsratios
oddsratio(death, 1, log=FALSE)
oddsratio(death, 2, log=FALSE)
oddsratio(death, 3, log=FALSE)
lor <- oddsratio(death, 3)
exp(confint(lor))
summary(lor)

plot(lor, xlab="Victim")



### Getting AC table for B = Victim (white)
death 
death[,,1] # victim = white
OddsRatioCI(death[,,1]) # victim = white
ChiSquareIndependence(death[,,1])

### Getting AC table for B = Victim (black)
death
death[,,2]
# in case of zero entries, 0.5 will be added to the table for calculations
OddsRatioCI(death[,,2])
ChiSquareIndependence(death[,,2], correct = TRUE)



##### Conditional on one variable

# first=death penalty (C), second=defendant race (A), third=victim race (B)
# GIVEN victim's race (white or black)
death[,,1] # white
oddsRatio(death[,,1])
death[,,2] # black
oddsRatio(death[,,2])
# GIVEN death penalty (yes or no)
death[1,,] # yes
oddsRatio(death[1,,])
death[2,,] # no
oddsRatio(death[2,,])
# GIVEN defendant's race (white or black)
death[,1,] # white
oddsRatio(death[,1,])
death[,2,] # black
oddsRatio(death[,2,])


##### Conditional on two variables

# GIVEN death penalty and victim's race
death[1,,1] # death=yes, victim=white
death[1,,2] # death=yes, victim=black
death[2,,1] # death=no, victim-white
death[2,,2] # death=no, victim=black

# GIVEN death penalty and defendant's race
death[1,1,] # death=yes, defend=white (we get victim's race white (19) and black (0))
death[1,2,] # death=yes, defend=black
death[2,1,] # death=no, defend=white
death[2,2,] # death=no, defend=black

# GIVEN defendant's race and victim's race
death[,1,1] # defend=white, victim=white
death[,1,2] # defend=white, victim=black
death[,2,1] # defend=black, victim=white
death[,2,2] # defend=black, victim=black



########################################################################################
## Another way to handle the same data
########################################################################################

defend <- rep(c("white", "black"), c(4,4)); defend
victim <- rep(rep(c("white", "black"), c(2,2)), 2); victim
penalty <- rep(c("yes", "no"), 4); penalty
count <- c(19, 132, 0, 9, 11, 52, 6, 97)
deathTable

# Table of defendant BY penalty
data <- xtabs(count ~ defend + penalty); data 
table <- list(Frequency=data, RowPerc=prop.table(data, 1)); table
ChiSquareIndependence(data, correct = TRUE)

# Table of defendant BY victim
data <- xtabs(count ~ defend + victim); data
table <- list(Frequency=data, Percent=prop.table(data), RowPerc=prop.table(data,1),
              ColPerc=prop.table(data,2)); table
ChiSquareIndependence(data, correct = TRUE)

# Table of victim BY penalty
data <- xtabs(count ~ victim + penalty); data
table <- list(Frequency=data, Percent=prop.table(data), RowPerc=prop.table(data,1),
              ColPerc=prop.table(data,2)); table
ChiSquareIndependence(data, correct = TRUE)
result <- chisq.test(data)
summary(result)


# Table of defendant by penalty GIVEN victim (VICTIM = BLACK)
data <- xtabs(count ~ penalty + defend + victim); data
data <- addmargins(data, margin=3)[,,1]; data
table <- list(Frequency=data, Percent=prop.table(data), RowPerc=prop.table(data,1),
              ColPerc=prop.table(data,2)); table
result <- chisq.test(data); result

oddsratio(table)

# Table of defendant by penalty  (VICTIM = white)
data3 <- xtabs(count ~ penalty + defend + victim); data3
data <- addmargins(data3, margin=3)[,,2]; data
table <- list(Frequency=data, Percent=prop.table(data), RowPerc=prop.table(data,1),
              ColPerc=prop.table(data,2)); table
result <- chisq.test(data); result



#### Mantel-Haenszel Test on victim * defend * penalty
mantelhaen.test(data3)

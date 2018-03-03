source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')

### Input the table
delinquent <- c("no","yes")
scout <- c("no", "yes")
SES <- c("low","med","high")
#table <- expand.grid(delinquent=c("yes","no"), scout=c("yes","no"),
table <- expand.grid(delinquent=delinquent, scout=scout, SES=SES)
#c(11,42,43,169, 14,20,104,132, 8,2,196,59)
table <- cbind(table, count=c(169,42,43,11,132,20,104,14,59,2,196,8))
table


TABLE <- expand.grid(delinquent=delinquent, scout=scout, SES=SES, 
                     purple=c("light","lavender","violet"),
                     pink=c("rose","peach","hotpink","magenta"))
TABLE <- cbind(TABLE, count=c(169,42,43,11,132,20,104,14,59,2,196,8))
tab <- xtabs(count ~ SES + delinquent + scout + purple + pink, TABLE); tab 



### Creating various sub-tables

# One-way Table SES
# (margin.table(temp.by.SES,3) = summing along levels of SES)
temp.by.SES <- xtabs(count ~ delinquent + scout + SES, table); temp.by.SES

Frequency <- as.vector(margin.table(temp.by.SES, 3)); Frequency # low,med,high
CumFreq <- cumsum(Frequency); CumFreq
cbind(SES, Frequency=Frequency, Percentage=Frequency/sum(Frequency),
      CumFrequency=CumFreq, CumPercentage=CumFreq/sum(Frequency))

# One-way Table scout
Frequency <- as.vector(margin.table(temp.by.SES, 2)); Frequency # low,med,high
CumFreq <- cumsum(Frequency); CumFreq
cbind(scout, Frequency=Frequency, Percentage=Frequency/sum(Frequency),
      CumFrequency=CumFreq, CumPercentage=CumFreq/sum(Frequency))

# One-way Table delinquent
Frequency <- as.vector(margin.table(temp.by.SES, 1)); Frequency # low,med,high
CumFreq <- cumsum(Frequency); CumFreq
cbind(delinquent, Frequency=Frequency, Percentage=Frequency/sum(Frequency),
      CumFrequency=CumFreq, CumPercentage=CumFreq/sum(Frequency))



##################### Test Mutual Independence model for THREE-WAY TABLE ####################
#############################################################################################

# (1) compute expected frequencies
A <- margin.table(temp.by.SES, 1); A
B <- margin.table(temp.by.SES, 2); B
C <- margin.table(temp.by.SES, 3); C
E <- (A %o% B %o% C) / sum(temp.by.SES)^2; E

# (2) compute X^2, and G^2
ChiSquareIndependence(temp.by.SES)
LikelihoodRatioTest(temp.by.SES)

MutualIndependence(temp.by.SES)


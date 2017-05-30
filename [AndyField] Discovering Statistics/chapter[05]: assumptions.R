
##install.packages("car") (did all)
##install.packages("pastecs")
##install.packages("psych")

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstats/andyfieldbookcode")

library(ggplot2)
library(car)
library(pastecs); #detach("package:pastecs")
library(psych) #detach("package:psych")


# Graph the data

dlf_outlier = read.delim("data/DownloadFestival.dat", header=TRUE)
tail(sort(dlf_outlier$day1))

# make new without outlier and replace the outlier
dlf = dlf_outlier
dlf[dlf==20.02] <- 2.02


# Day 1
hist.day1 = ggplot(dlf, aes(day1)) + 
  geom_histogram(aes(y=..density..), colour="white", fill="dodgerblue") +
  labs(x="Hygiene score on day 1", y="Density") +
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(dlf$day1, na.rm=TRUE), 
                  sd = sd(dlf$day1, na.rm=TRUE)),
                colour="black", 
                size=1)
hist.day1

# Day 2
hist.day2 = ggplot(dlf, aes(day2)) + 
  geom_histogram(aes(y=..density..), colour="white", fill="dodgerblue") + 
  labs(x="Hygiene Day 2", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(dlf$day2, na.rm=TRUE),
                  sd=sd(dlf$day2, na.rm=TRUE)), size=1)
hist.day2

# Day 3
hist.day3 = ggplot(dlf, aes(day3)) + 
  geom_histogram(aes(y=..density..), colour="white", fill="dodgerblue") + 
  labs(x="Hygiene Day 3", y="Density") +
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(dlf$day3, na.rm=TRUE),
                  sd=sd(dlf$day3, na.rm=TRUE)), size=1)
hist.day3

# QQplot
qqplot.day1 = qplot(sample=dlf$day1, stat="qq")
qqplot.day1
qqplot.day2 = qplot(sample=dlf$day2, stat="qq")
qqplot.day2
qqplot.day3 = qplot(sample=dlf$day3, stat="qq")
qqplot.day3


# Quantify normality
describe(dlf$day1)
stat.desc(cbind(dlf$day1), basic=FALSE, norm=TRUE) # OR

describe(cbind(dlf$day1, dlf$day2, dlf$day3))
round(stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic=F, norm=T), 3)



# R functions
rexam = read.delim("data/RExam.dat", header=TRUE)
rexam$uni = factor(rexam$uni, levels=c(0:1), 
                   labels=c("Dunceton University", 
                            "Sussex University"))
rexam

stat.desc(cbind(rexam$exam, rexam$computer, rexam$lectures, rexam$numeracy), basic=FALSE, norm=TRUE)


# First Year Exam Scores
hist.firstexam = ggplot(rexam, aes(rexam$exam)) +
  geom_histogram(aes(y=..density..), fill="lightskyblue") + 
  labs(x="First Year Exam Scores", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(rexam$exam, na.rm=T), 
                  sd=sd(rexam$exam, na.rm=T)), 
                size=1, col="dodgerblue")

# Computer Literacy
hist.computer = ggplot(rexam, aes(rexam$computer)) +
  geom_histogram(aes(y=..density..), fill="lightskyblue") + 
  labs(x="Computer Literacy", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(rexam$computer, na.rm=T), 
                  sd=sd(rexam$computer, na.rm=T)), 
                size=1, col="dodgerblue")

# Percentage of Lectures Attended
hist.lectures = ggplot(rexam, aes(rexam$lectures)) +
  geom_histogram(aes(y=..density..), fill="lightskyblue") + 
  labs(x="Percentage of Lectures Attended", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(rexam$lectures, na.rm=T), 
                  sd=sd(rexam$lectures, na.rm=T)), 
                size=1, col="dodgerblue")


# Numeracy
hist.numeracy = ggplot(rexam, aes(rexam$numeracy)) +
  geom_histogram(aes(y=..density..), fill="lightskyblue") + 
  labs(x="Numeracy", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(rexam$numeracy, na.rm=T), 
                  sd=sd(rexam$numeracy, na.rm=T)), 
                size=1, col="dodgerblue")


#multiplot(hist.firstexam, hist.computer, hist.lectures, hist.numeracy, cols=2)



# Grouped data

describe(rexam)
round(stat.desc(cbind(rexam$exam, rexam$computer, rexam$lectures, rexam$numeracy), basic=F, norm=T), 3)

# Separate groups

by(data=rexam$exam, INDICES=rexam$uni, FUN=describe)
by(data=rexam$exam, INDICES = rexam$uni, FUN=stat.desc, 
   basic=FALSE, norm=TRUE)

by(data=rexam[, c("exam", "numeracy")], INDICES = rexam$uni, 
   FUN=stat.desc, basic=FALSE, norm=TRUE)
by(cbind(data=rexam$exam, data=rexam$numeracy), 
   INDICES = rexam$uni, stat.desc, basic=FALSE, norm=TRUE)
by(cbind(data=rexam$exam, data=rexam$numeracy), 
   INDICES = rexam$uni, describe)


# Subsetting
dunceData = subset(rexam, rexam$uni=="Dunceton University")
sussexData = subset(rexam, rexam$uni=="Sussex University")

hist.numeracy.dunceton = ggplot(dunceData, aes(numeracy)) + 
  geom_histogram(aes(y=..density..), fill="mediumpurple3", binwidth=1) +
  labs(x="Numeracy score", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(dunceData$numeracy, na.rm=TRUE),
                  sd=sd(dunceData$numeracy, na.rm=TRUE)),
                colour="black", size=1)
hist.numeracy.dunceton

hist.exam.dunceton = ggplot(dunceData, aes(exam)) + 
  geom_histogram(aes(y=..density..), fill="mediumpurple3", binwidth=1) +
  labs(x="Exam score", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(dunceData$exam, na.rm=TRUE),
                  sd=sd(dunceData$exam, na.rm=TRUE)),
                colour="black", size=1)
hist.exam.dunceton

hist.numeracy.sussex = ggplot(sussexData, aes(numeracy)) + 
  geom_histogram(aes(y=..density..), fill="mediumpurple3", binwidth=1) +
  labs(x="Numeracy score", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(sussexData$numeracy, na.rm=TRUE),
                  sd=sd(sussexData$numeracy, na.rm=TRUE)),
                colour="black", size=1)
hist.numeracy.sussex

hist.exam.sussex = ggplot(sussexData, aes(exam)) + 
  geom_histogram(aes(y=..density..), fill="mediumpurple3", binwidth=1) +
  labs(x="Exam score", y="Density") + 
  stat_function(fun=dnorm, 
                args=list(
                  mean=mean(sussexData$exam, na.rm=TRUE),
                  sd=sd(sussexData$exam, na.rm=TRUE)),
                colour="black", size=1)
hist.exam.sussex



# Testing normality

# all groups together
shapiro.test(rexam$exam) # p-value < 0.05, so data is not normal
shapiro.test(rexam$numeracy)

hist.numeracy
hist.firstexam

qplot(sample=rexam$exam, stat="qq")
qplot(sample=rexam$numeracy, stat="qq")

# separating the groups

# exam score
by(rexam$exam, rexam$uni, shapiro.test)
hist.exam.dunceton
hist.exam.sussex

qplot(sample=dunceData$exam, stat="qq")
qplot(sample=sussexData$exam, stat="qq")

# numeracy
by(rexam$numeracy, rexam$uni, shapiro.test)
hist.numeracy.dunceton
hist.numeracy.sussex

qplot(sample=dunceData$numeracy, stat="qq")
qplot(sample=sussexData$numeracy, stat="qq")




# Testing homogeneity of variance

# leveneTest(outcome variable, group, center=median or mean)
#default center is the median
leveneTest(rexam$exam, rexam$uni, center=mean)
hist.exam.dunceton
hist.exam.sussex

leveneTest(rexam$numeracy, rexam$uni, center=mean) 
hist.numeracy.dunceton
hist.numeracy.sussex




# Transforming Data

# Positive skew, unequal variances: 
# 1) log(x_i) 
# 2) sqrt(x_i)
# 3) 1/x_i

# Negative skew: 
# any of the above work but reverse the scores: subtract each 
#score from the highest. 
hist.day1
hist.day2
hist.day3

# useful functions
rowMeans(cbind(dlf$day1, dlf$day2, dlf$day3, na.rm=TRUE))
rowSums(cbind(dlf$day1, dlf$day2, dlf$day3, na.rm=TRUE))
sqrt(dlf$day2)
abs(dlf$day1)
dlf$day1
log10(dlf$day1)
log(dlf$day1) # natural logarithm
mean(is.na(dlf$day2))
# if value > 4, switch to NA but if not, keep as day1 value
dlf$day1NoOutlier = ifelse(dlf$day1 > 4, NA, dlf$day1)

head(dlf)
# LOG transformation
dlf$logday1 = log(dlf$day1 + 1) # add 1 since there is 0 in day2
dlf$logday2 = log(dlf$day2 + 1)
dlf$logday3 = log(dlf$day3 + 1)
# dlf$logday1 = NULL # how to delete a column

# SQRT transformation
dlf$sqrtday1 = sqrt(dlf$day1)
dlf$sqrtday2 = sqrt(dlf$day2)
dlf$sqrtday3 = sqrt(dlf$day3)

# RECIPROCAL transformation
dlf$recday1 = 1/(dlf$day1 + 1)
dlf$recday2 = 1/(dlf$day2 + 1)
dlf$recday3 = 1/(dlf$day3 + 1)


# day1
m1 = matrix(data=cbind(dlf$day1, dlf$logday1, dlf$sqrtday1, dlf$recday1), ncol=4)
colnames(m1) = c("day1", "logday1", "sqrtday1", "recday1")
dfs1 = stack(as.data.frame(m1))

# day2
m2 = matrix(data=cbind(dlf$day2, dlf$logday2, dlf$sqrtday2, dlf$recday2), ncol=4)
colnames(m2) = c("day2", "logday2", "sqrtday2", "recday2")
dfs2 = stack(as.data.frame(m2))

# day3
m3 = matrix(data=cbind(dlf$day3, dlf$logday3, dlf$sqrtday3, dlf$recday3), ncol=4)
colnames(m3) = c("day3", "logday3", "sqrtday3", "recday3")
dfs3 = stack(as.data.frame(m3))


# Densities multiplot
day1Trans = ggplot(dfs1, aes(x=values)) + geom_density(aes(group=ind, color=ind), size=1)
day2Trans = ggplot(dfs2, aes(x=values)) + geom_density(aes(group=ind, color=ind), size=1)
day3Trans = ggplot(dfs3, aes(x=values)) + geom_density(aes(group=ind, color=ind), size=1)
day1Trans
day2Trans
day3Trans

# Dealing with missing scores
# put NA if there is any NA in the row
dlf$meanHygiene = rowMeans(cbind(dlf$day1, dlf$day2, dlf$day3))
# do the mean of available values in the row
dlf$meanHygiene = rowMeans(cbind(dlf$day1, dlf$day2, dlf$day3), na.rm=TRUE)
dlf$meanHygiene
# do the mean of scores with missing days 2 or more
dlf$daysMissing = rowSums(cbind(is.na(dlf$day1), 
                                is.na(dlf$day2), 
                                is.na(dlf$day3)))
dlf$meanHygiene = ifelse(dlf$daysMissing < 2, NA, 
                         rowMeans(cbind(dlf$day1, 
                                        dlf$day2, 
                                        dlf$day3), 
                                  na.rm=TRUE))
head(dlf)



# Functions I Made ----------------------------------------------

skew <- function(x) {
  m3 <- sum((x - mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  return(m3/s3)
}
skew.test = function(x){
  skew.statistic <- skew(x)/sqrt(6/length(x))
  p.value = 1 - pt(skew.statistic, df=(length(x) - 2))
  return(invisible(cat("\n\n************* Skew Test *************
  \n     Skew: ", round(skew(x), 5), "
     Skew-statistic: ", round(skew.statistic, 5), 
                       "\n     P-value: ", round(p.value, 5), "\n\n")))
}

kurtosis = function(x) {
  m4 = sum((x - mean(x))^4)/length(x)
  s4 = sqrt(var(x))^4
  return(m4/s4-3)
}
kurtosis.test = function(x) {
  kurtosis.statistic = kurtosis(x)/sqrt(24/length(x))
  p.value = 1 - pt(kurtosis.statistic, df=(length(x)-2))
  return(invisible(cat("\n\n************* Kurtosis Test *************
  \n     Kurtosis: ", round(kurtosis(x), 5), "
     Kurtosis-statistic: ", round(kurtosis.statistic, 5), 
                       "\n     P-value: ", round(p.value, 5), "\n\n")))
}

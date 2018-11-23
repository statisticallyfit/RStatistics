library(ggplot2)

#install.packages("DSUR")
# library(DSUR)

setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/[AndyField] Discovering Statistics/")



# SCATTERPLOT 

facebookData <- read.delim("data/FacebookNarcissism.dat", header=TRUE)
head(facebookData)

graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))
graph + geom_point()
graph + geom_point(shape=19, 
                   col="dodgerblue", size=3)
graph + geom_point(shape=19, 
                   aes(colour=Rating_Type))
graph + geom_point(shape=19, 
                   aes(colour=Rating_Type),
                   position="jitter")
#graph + geom_point(aes(shape=Rating_Type),
#                   position="jitter")

graph <-ggplot(facebookData, aes(NPQC_R_Total, Rating, colour=Rating))
graph + geom_point(shape=19)

# ------------------
examData <- read.delim("data/Exam Anxiety.dat", header=TRUE)
head(examData)

scatter <- ggplot(examData, aes(Anxiety, Exam))

scatter + 
  geom_point(shape=19) + 
  labs(x="Exam Anxiety", y="Exam Performance %") + 
  geom_smooth(method="lm", lwd=1, se=TRUE) # can switch off shade around line by saying se=FALSE

# shade is colored
scatter + 
  geom_point(shape=19) + 
  labs(x="Exam Anxiety", y="Exam Performance %") + 
  geom_smooth(method="lm", lwd=1, alpha=0.1, fill="blue") # can switch off shade around line by saying se=FALSE

# study genders separately
scatter <- ggplot(examData, aes(Anxiety, Exam, colour=Gender))
# use fill=Gender to specify different colored confidence interval for genders
# using aes() because we are specifying variable not single colour
scatter + geom_point(shape=19) + geom_smooth(method="lm", lwd=1, 
                                             aes(fill=Gender), 
                                             alpha=0.1)



# HISTOGRAM

festivalData = read.delim("data/DownloadFestival.dat", header=TRUE)
head(festivalData)

hist <- ggplot(festivalData, aes(day1)) # opts(legend.position="none) did not work
hist + 
  geom_histogram(binwidth=0.4, fill="navy") + 
  labs(x="Hygiene (Day 1 of festival)", y="Frequency")




# BOXPLOT

boxplot <- ggplot(festivalData, aes(gender, day1))
boxplot + geom_boxplot() + labs(x="Gender", y="Hygiene (Day 1 of festival)")

# Who is the outlier?
which(festivalData[,3]==20.02) # execute before ordering it
# OR
orderedFestivalData <- festivalData[order(festivalData$day1),]
festivalDataWithoutOutlier <- orderedFestivalData[-810,]
nrow(festivalData)
nrow(festivalDataWithoutOutlier)

boxplot <- ggplot(festivalDataWithoutOutlier, aes(gender, day1))
boxplot + geom_boxplot() 

hist <- ggplot(festivalDataWithoutOutlier, aes(day1))
hist + geom_histogram(fill="lightslateblue")



# DENSITY PLOT: similar to histogram

density <- ggplot(festivalDataWithoutOutlier, aes(day1))
density + geom_density(lwd=1) + labs(x="Hygiene of Day1", y="Density Estimate")

source('/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/StatsFormulas.R')
setwd("/datascience/projects/statisticallyfit/github/learningstatistics/RStatistics/learnstatistics/[PennState] GeneralizedLinearModels/BinaryLogisticRegression")

donner <- read.table("donner.txt")
colnames(donner) <- c("Age", "Gender", "Survival")
# Age, Gender (male=1, female=0), Surival (survived=1, death=0)
head(donner)
attach(donner)

table <- as.matrix(table(Gender, Survival))
table 

library(ggplot2)
library(Hmisc); #detach("package:Hmisc")
library(ggm); #detach("package:ggm")
library(corrplot)
library(boot)
library(polycor)

setwd("/datascience/projects/statisticallyfit/github/learningprogramming/R/RStats/learnstatistics/DiscoveringStatswithR")
getwd()

#install.packages("polycor")
#install.packages("corrplot")
##install.packages("Hmisc") # installed first 2, not last
##install.packages("ggm")
#install.packages("polycor")


toffeeData = read.delim("data/Advert.dat")
scatter = ggplot(toffeeData, aes(x=adverts, y=packets)) + geom_point(shape=19)
scatter

r = cor(toffeeData$adverts, toffeeData$packets); r
cor.test(toffeeData$adverts, toffeeData$packets)

# Testing significance of correlation coefficient
# NOTE: r sampling distribution is non-normal but the below
# transformation by Fisher makes it normal
# z_r = (1/2)* ln((1+r)/(1-r))
# SE_zr = 1/sqrt(N-3)
# statistic: z = Zr/SE_Zr
Zr = 0.5 * log((1+r)/(1-r))
n = length(toffeeData$adverts)
SE_Zr = 1/sqrt(n-3)
z = Zr/SE_Zr; z
p.value.one.tailed = 1 - pnorm(z)
p.value.two.tailed = 2*(1 - pnorm(z))

# R does this test with t-statistic:
# Tr = r*sqrt(n-2)/sqrt(1-r^2)
Tr = r*sqrt(n-2)/sqrt(1-r^2)
1 - pt(Tr, df=n-1)

# Confidence interval for Zr
confIntZr = c(Zr - 1.96*SE_Zr, Zr + 1.96*SE_Zr)
confIntZr
# convert back to r confint = (e^(2*Zr) - 1, e^(2*Zr) + 1)
rLower = (exp(2*confIntZr[1]) - 1)/(exp(2*confIntZr[1]) + 1)
rUpper = (exp(2*confIntZr[2]) - 1)/(exp(2*confIntZr[2]) + 1)
confIntR = c(rLower, rUpper); confIntR



# Correlation 
examData = read.delim("data/Exam Anxiety.dat", header=TRUE)

# use: 
# (1) everything - puts NA if something missing
# (2) all.obs - error if something missing
# (3) complete.obs - excluding cases listwise
# (4) pairwise.complete.obs - excluding cases pairwise
cor(examData$Exam, examData$Anxiety, use="complete.obs", method="pearson")
cor(examData$Exam, examData$Anxiety, use="complete.obs", method="kendall")
cor(examData$Exam, examData$Anxiety, use="pairwise.complete.obs", method="kendall")


# rcorr() does pairwise exclusion; unchangeable
rcorr(as.vector(examData$Exam), as.vector(examData$Anxiety), type="pearson")
rcorr(examData, type="pearson")

# cor.test is another way to find correlation coefficient
cor.test(examData$Exam, examData$Anxiety, alt="less", 
         method="pearson", conf.level=0.99)

# correlation matrix with cor()
cor(examData[, 1:4])





# Pearson's R

examData2 = examData[, c("Exam", "Anxiety", "Revise")]
r = cor(examData2$Exam, examData2$Anxiety, method="pearson"); r
# method 1 for cor matrix
cor(examData2)
# method 2 for cor matrix
examMatrix = as.matrix(examData2) 
rcorr(examMatrix)
# method 3 for cor matrix
r.mat = cor(examMatrix); r.mat
# visualize the correlation matrix with correlogram: 
corrplot(cor.mat, type="upper", order="hclust", tl.col="black", tl.srt=45)

# look at confidence interval of correlation for Exam and Anxiety
cor.test(examData$Anxiety, examData$Exam)$conf.int
cor.test(examData$Anxiety, examData$Revise)$conf.int
cor.test(examData$Revise, examData$Exam)$conf.int

# THis means exam performance shares 19.44% variability of anxiety
rsquared.mat = cor(examData2)^2 * 100
rsquared.mat



# Spearman's Rho: apply Pearson's R to ranked data

# put file.choose() instead if you want to click on the file
liarData = read.delim("data/The Biggest Liar.dat", header=TRUE)
# correlation matrix
cor(liarData, method="spearman")
# correlation coefficient
rho = cor(liarData$Position, liarData$Creativity, method="spearman")
rho



# Kendall's Tau: apply if many scores have same rank (and data is small)
# more accurate than spearman's method

tau = cor(liarData$Position, liarData$Creativity, method="kendall")
tau
cor.test(liarData$Position, liarData$Creativity, alt="less", method="kendall")



# Bootstrapping Correlations to find R when assumptions aren't met
# For how bootstrapping works, see: 
# www.mayin.org/ajayshah/KB/R/documents/boot.html

# KENDALL method
bootTau = function(liarData, indices) 
      cor(liarData$Position[indices], liarData$Creativity[indices], use="complete.obs", method="kendall")
bootKendallInfo = boot(liarData, bootTau, 2000) # 2000 is sample size
bootKendallInfo # bias in the tau is small, stderror is based on bootstrapped samples
head(bootKendallInfo$t) # all the correlation estimates
boot.ci(bootKendallInfo) # gives four different conf ints


# PEARSON method

bootR = function(examData2, indices) 
      cor(examData2$Exam[indices], examData2$Anxiety[indices], use="complete.obs", method="pearson")
bootPearsonInfo = boot(examData2, bootR, 2000)
head(bootPearsonInfo$t) # the correlation estimates - all 2000
boot.ci(bootPearsonInfo) 

bootR = function(examData2, i) cor(examData2$Revise[i], examData2$Anxiety[i], use="complete.obs", method="pearson")
bootPearsonInfo = boot(examData2, bootR, 2000)
head(bootPearsonInfo$t)
boot.ci(bootPearsonInfo) 

bootR = function(examData2, i) cor(examData2$Exam[i], examData2$Revise[i], use="complete.obs", method="pearson")
bootPearsonInfo = boot(examData2, bootR, 2000)
head(bootPearsonInfo$t)
boot.ci(bootPearsonInfo) 


# SPEARMAN method

bootRho = function(examData2, i) cor(examData2$Exam[i], examData2$Anxiety[i], use="complete.obs", method="spearman")
bootSpearmanInfo = boot(examData2, bootRho, 2000)
head(bootSpearmanInfo$t)
boot.ci(bootSpearmanInfo)

bootRho = function(examData2, i) cor(examData2$Revise[i], examData2$Anxiety[i], use="complete.obs", method="spearman")
bootSpearmanInfo = boot(examData2, bootRho, 2000)
bootSpearmanInfo
boot.ci(bootSpearmanInfo)

bootRho = function(examData2, i) cor(examData2$Exam[i], examData2$Revise[i], use="complete.obs", method="spearman")
bootSpearmanInfo = boot(examData2, bootRho, 2000)
bootSpearmanInfo
boot.ci(bootSpearmanInfo)



# Point biserial correlation: two separate categories

catData = read.csv("data/pbcorr.csv", header=TRUE); head(catData)
r.pb = cor(catData$time, catData$gender, method="pearson"); r.pb
cor(catData$time, catData$recode) # sign has no meaning
cor.test(catData$time, catData$gender, method="pearson")
r.pb^2 # gender accounts for this % of time spent away from home

# Biserial correlation (cats were neutered so there 
# are more than 2 categories)
# CONVERT: r.b = r.pb * sqrt(pq)/y
# y = ordinate of normal distribution at the point where there
# is p% of the area on one side and q% on the other side

# LONG WAY calculation
catFreq = table(catData$gender); catFreq
props = prop.table(catFreq); p = props[1]; q = props[2]; props
z.props = qnorm(p); z.props
y = dnorm(z.props); y # y is ordinate value determined by proprtions
r.b = as.numeric(r.pb * sqrt(p*q)/y); r.b
# SHORT WAY calculation
polyserial(catData$time, catData$gender)
r.b

# Significance of biserial correlation
# SE.r.b = sqrt(p*q)/(y*sqrt(n))
# z.r.b = (rb - rb.bar)/SE.r.b
n = nrow(catData); n
SE.rb = as.numeric(sqrt(p*q)/(y*sqrt(n))); SE.rb
z.rb = r.b/SE.rb; z.rb
p.value = 2*(1 - pnorm(z.rb)); p.value

shapiro.test(catData$time) # assumptions of normality don't hold... ?




# Partial correlations

r.mat
rsquared.mat
# we are finding pure correlation between exam and anxiety
# the effect the Revise has on both Exam and Anxiety is controlled
pc = pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc; pc^2
# pcor.test(pcor object, number of control variables, sample size)
pcor.test(pc, 1, 103) #nrow(examData2) = 103

# Semi-partial correlations

# Here, we would control for the effect that Revise has on only one 
# of the other variables (either Exam or Anxiety but not both)




# Comparing Correlations (between male and female)

maleExam = subset(examData, Gender == "Male", select=c("Exam", "Anxiety"))
femaleExam = subset(examData, Gender == "Female", select=c("Exam", "Anxiety"))
rMale = cor(maleExam$Exam, maleExam$Anxiety); rMale
rFemale = cor(femaleExam$Exam, femaleExam$Anxiety); rFemale

# Compare the INDEPENDENT correlation coefficients
# z_r = (1/2)* ln((1+r)/(1-r))
# SE_zr = 1/(N-3)
# statistic: zDiff = (zr1 - zr2)/sqrt(1/(N1-3) + 1/(N2-3))

zrm = (1/2) * log((1 + rMale)/(1 - rMale))
zrf = (1/2) * log((1 + rFemale)/(1 - rFemale))
Nm = nrow(maleExam)
Nf = nrow(femaleExam)
zDiff = (zrm - zrf)/sqrt(1/(Nm - 3) + 1/(Nf - 3)); zDiff
p.value = pnorm(zDiff); p.value
# CONCLUDE: no significant difference in correlation between men and women


# Comparing DEPENDENT correlation coefficients (comparing different r's
# that come from same sample)
# t-stat is used to test for sig diff between *two* dependent r's
# FORMULA: tDiff = (rxy - rzy) * sqrt(((n-3)(1 + rxz))/(2(1 - rxy^2 - rxz^2 - rzy^2 + 2*rxy*rzx*rzy)))

# Test if relation of anxiety (x) and exam (y) is stronger than
# relation between revision (z) and exam (y)
rXY = cor(examData2$Anxiety, examData2$Exam); rXY
rZY = cor(examData2$Revise, examData2$Exam); rZY
rXZ = cor(examData2$Anxiety, examData2$Revise); rXZ
N = nrow(examData2); N
tDiff = (rXY - rZY) * sqrt((N-3)*(1 + rXZ)/(2*(1 - rXY^2 - rXZ^2 - rZY^2 + 2*rXY*rXZ*rZY))); tDiff
p.value = pt(tDiff, df=(N-3)); p.value
# CONCLUDE: significant difference since p.value < 0.05

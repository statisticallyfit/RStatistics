setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/COSTENG.Rdata")


COSTENG$ENGINEER <- factor(COSTENG$ENGINEER)
COSTENG$JOB <- factor(COSTENG$JOB)

# block means: 
with(COSTENG, tapply(COST, INDEX=JOB, mean))
# treatment means (engineer):
with(COSTENG, tapply(COST, INDEX=ENGINEER, mean))

par(mfrow=c(1,1))
with(COSTENG, interaction.plot(response=COST, x.factor=ENGINEER, trace.factor = JOB))


engjob.lm <- lm(COST ~ JOB + ENGINEER, data=COSTENG)
summary(engjob.lm)
anova(engjob.lm)
# No difference in mean cost among engineers (p = 0.07) once jobs has been accounted
# Difference in mean cost among jobs


# From anova term p = 0.07 can expect confint between engineers to contain 0 
# (no diff)
# Cont int between eng1 and eng2. (see lecture)
with(COSTENG, tapply(COST, INDEX=ENGINEER, mean))




engjob.null.lm <- lm(COST ~ 1, data=COSTENG)
# block test- make block params = 0, so just have treatment variable (engineers)
engjob.blockTest.lm <- lm(COST ~ ENGINEER, data=COSTENG)
# treatment test - make treat params = 0, so just have the block variable (jobs)
engjob.treatTest.lm <- lm(COST ~ JOB, data=COSTENG)


# global F-test
anova(engjob.null.lm, engjob.lm, test="Chisq") # last row, 2nd last col is resid dev
NestedFTest(engjob.null.lm, engjob.lm)

# nested F-test for test block means (reduced = treatments)
anova(engjob.blockTest.lm, engjob.lm) # so there is significant mean difference
# among the different blocks (jobs)
NestedFTest(engjob.blockTest.lm, engjob.lm)

# nested F-test for test treatment means (reduced = block params)
anova(engjob.treatTest.lm, engjob.lm) # no significant mean difference among
# the different engineers (treatments)
NestedFTest(engjob.treatTest.lm, engjob.lm)


anova(engjob.lm)


# CM
b = 4; # num blocks = 4
p = 3; # num treatments = 3
n <- nrow(COSTENG)
CM = sum(COSTENG$COST)^2/n; CM
SS_total = SSyy(engjob.lm);
SS_total
sum(COSTENG$COST^2)-CM
#sst (sum of squares for treatments)
T1 <- sum(subset(COSTENG, ENGINEER=="1")$COST)
T2 <- sum(subset(COSTENG, ENGINEER=="2")$COST)
T3 <- sum(subset(COSTENG, ENGINEER=="3")$COST)
sst = T1^2/b + T2^2/b + T3^2/b - CM; sst
# ssb (sum of squares for blocks)
B1 <- sum(subset(COSTENG, JOB == "1")$COST)
B2 <- sum(subset(COSTENG, JOB == "2")$COST)
B3 <- sum(subset(COSTENG, JOB == "3")$COST)
B4 <- sum(subset(COSTENG, JOB == "4")$COST)
ssb = B1^2/p + B2^2/p + B3^2/p + B4^2/p - CM; ssb
# sse
sse = SS_total - sst - ssb; sse

# same as results here: 
anova(engjob.lm)


Fstat.treat = (sst/(p-1))/(sse/(n-p-b+1)); Fstat.treat
Fstat.block = (ssb/(b-1))/(sse/(n-p-b+1)); Fstat.block


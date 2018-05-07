setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

options(digits=10, show.signif.stars = FALSE)
load("data/Exercises and Examples/COSTENG.Rdata")


COSTENG$ENGINEER <- factor(COSTENG$ENGINEER)
COSTENG$JOB <- factor(COSTENG$JOB)

engjob.lm <- lm(COST ~ ENGINEER + JOB, data=COSTENG)
summary(engjob.lm)
engjob.glm <- glm(COST ~ ENGINEER + JOB, data=COSTENG)
summary(engjob.glm)

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

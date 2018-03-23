setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/")

load("data/Exercises and Examples/FLAG2.Rdata")
options(digits=10)


flag.all <- lm(LOWBID ~ DOTEST + LBERATIO + STATUS + DISTRICT + NUMBIDS + 
                     DAYSEST + RDLNGTH + PCTASPH + PCTBASE + PCTEXCAV + 
                     PCTMOBIL + PCTSTRUC + PCTTRAFF + SUBCONT, data = FLAG2)
summary(flag.all)

flag.start <- lm(LOWBID ~ 1, data=FLAG2)

flag.stepwise <- step(flag.start, scope=formula(flag.all), test="F")
formula(flag.stepwise)
summary(flag.stepwise)

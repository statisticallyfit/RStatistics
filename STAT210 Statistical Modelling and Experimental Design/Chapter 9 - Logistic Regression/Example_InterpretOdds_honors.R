setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 9 - Logistic Regression/")

# source: 
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/


honorsData <- read.table("honors.txt", header=TRUE)

honor.0.glm <- glm(hon ~ 1, family=binomial, data=honorsData)
summary(honor.0.glm)
# INTERPRET: so ln(p/(1-p)) = -1.12546




# MAKING FREQUENCY TABLE
# hon = 1 (student takes honors), hon = 0 (not)
freq <- rbind(table(honorsData$hon), 
              100*table(honorsData$hon)/nrow(honorsData))
freq <- data.frame(freq)
colnames(freq) <- c("No", "Honors")
rownames(freq) <- c("Freq", "Percent")
freq <- t(freq)
freq

# so P(y=1) = P(honors) = 0.245
# so ln(0.245/(1-0.245)) = -1.12546
honor.0.glm$family$linkfun(0.245) 

# transform back: 
intercept <- summary(honor.0.glm)$coefficients[1,1]
honor.0.glm$family$linkinv(intercept)





# MODEL 2 - one predictor (female)
honor.female.glm <- glm(hon ~ female, data=honorsData, family="binomial")
summary(honor.female.glm)
cof <- summary(honor.female.glm)$coef

attach(honorsData)
tab <- table(hon, female)
tab <- marginalTable(tab) # female (1), male(0)

od <- oddsRatio(tab)
od[1,1] - 1
# so the odds for females are about 80.9% higher than odds for males


oddsMales <- tab[2,1]/tab[1,1]; oddsMales
oddsFem <- tab[2,2] / tab[1,2]; oddsFem
oddsFem/oddsMales


# the intercept = ln(odds for males in honors)
log(oddsMales)
cof[1,1]

# the predictor slope for females is the log of odds between fem and males
log(oddsFem/oddsMales)
cof[2,1]
exp(cof) # note exponentiating stderrors doesn't make sense. 




# Math honors
honor.math.glm <- glm(hon ~ math, data=honorsData, family=binomial)
cof <- summary(honor.math.glm)$coef
cof
# the coefficient is the log odds of a student with math score of 0 being
# in an honors class. 
exp(cof) # odds of being in honors when math = 0 is 0.0000557


# on the math model
#https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/Chapter 9 - Logistic Regression/")

# source: 
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/


honorsData <- read.table("honors.txt", header=TRUE)

# MAKING FREQUENCY TABLE
# hon = 1 (student takes honors), hon = 0 (not)
freq <- rbind(table(honorsData$hon), 
              100*table(honorsData$hon)/nrow(honorsData))
freq <- data.frame(freq)
colnames(freq) <- c("No", "Honors")
rownames(freq) <- c("Freq", "Percent")
freq <- t(freq)
freq



# MODEL 1 ----------------------------------------------------------------------------
honor.0.glm <- glm(hon ~ 1, family=binomial, data=honorsData)
summary(honor.0.glm)
# INTERPRET: so ln(p/(1-p)) = -1.12546
# so P(y=1) = P(honors) = 0.245
# so ln(0.245/(1-0.245)) = -1.12546
honor.0.glm$family$linkfun(0.245) 
summary(honor.0.glm)$coef # same as the coef
# INTERPRET: 0.245 is the odds of being in honors class for the entire population.
# INTERPRET -1.125 is the log odds of being in honors class

# transform back: 
intercept <- summary(honor.0.glm)$coefficients[1,1]
honor.0.glm$family$linkinv(intercept)




# MODEL 2 - one predictor (female) ------------------------------------------------------
honor.female.glm <- glm(hon ~ female, data=honorsData, family="binomial")
summary(honor.female.glm)
cof <- summary(honor.female.glm)$coef

tab <- table(honorsData$hon, honorsData$female)
colnames(tab) <- c("Male", "Female")
rownames(tab) <- c("No", "Honors")
m.tab <- marginalTable(tab); m.tab 

oddsRatio(tab)
oddsRatio(tab) - 1
# so the odds for females are about 80.9% higher than odds for males

rowOdds(tab)
colOdds(tab)

oddsMales <- tab[2,1]/tab[1,1]; oddsMales
oddsFem <- tab[2,2] / tab[1,2]; oddsFem
oddsFem/oddsMales


# the intercept = ln(odds for males in honors since males are the base group)
log(oddsMales)
cof 
log(oddsFem/oddsMales) # mu.fem = slope.fem - intercept => log division. 
# the coeff is = ln(oddsFem / oddsMales) since here differences are expressed
# as logs (division)
exp(cof[2,1])-1 - exp(cof[1,1]) -1 # real mu.fem? 


# the predictor slope for females is the log of odds between fem and males
log(oddsFem/oddsMales)
cof[2,1]
exp(cof) # note exponentiating stderrors doesn't make sense. 




# MODEL 3: Math honors -------------------------------------------------------------------
# relation between math score and log odds of being in honors class. 
honor.math.glm <- glm(hon ~ math, data=honorsData, family=binomial)
cof <- summary(honor.math.glm)$coef
cof

# INTERPRET: intercept
# the coefficient is the log odds of a student with math score of 0 being
# in an honors class. 
exp(cof) # odds = 0.0000557 of being in honors when math =0 . (so very low)

# INTERPRET :COEF math
exp(cof) - 1
cof
# Means that for a 1-unit increase in math score, there is 17% increase (16.9%)
# in odds of being in honors class. 




# MODEL 4: logistic multiple predictors (math, fem, read) -----------------------------
honor.math.fem.read.glm <- glm(hon ~ math + female + read, 
                               family=binomial, data=honorsData)
cof <- summary(honor.math.fem.read.glm)$coef
cof

exp(cof)
# INTERPRET: 
# --- holding math,reading fixed, the odds of getting into honors for females
# divided by the odds of getting into honors for males = odds ratio = 2.66
# --- holding fem,reading fixed, the odds ratio of getting into honors versus 
# not getting into honors, for math is 1.13 (13% increase in odds of getting
# into honors for a 1 unit increase in math score)
# --- holding fem, math fixed, the odds ratio of getting into honors versys not
# getting into honors for reading is 1.06 (6% increase in odds of getting
# into honors for 1 unit increase in reading score)






# MODEL 5: interaction of two predictors -----------------------------------------------
honor.interact.glm <- glm(hon ~ female*math, family=binomial,data=honorsData)
cof <- summary(honor.interact.glm)$coef
cof
exp(cof)
# INTERPRET: 
# males (x1=0): log(p/q) = β0 + β2*math
# females (x1=1): log(p/q) = (β0 + β1) + (β2 + β3)*math
#  --- for males (x1=0), a 1-unit increase in math score yields change in log
# odds of 0.129. So for males, the odds ratio is e^0.13 = 1.14 for a 1 unit
# increase in math score.
# --- for females, a 1-unit increase in math score yields change in log odds
# of about (0.129 + 0.0669) = 0.19637, so the odds ratio is e^0.196 = 1.22 for 1unit
# increase in math score. 
# --- INTERACTION TERM: ratio of female honors for math (odds) over male honors
# for math (odds) = 1.22/1.14 = exp(0.0669) = 1.07. 
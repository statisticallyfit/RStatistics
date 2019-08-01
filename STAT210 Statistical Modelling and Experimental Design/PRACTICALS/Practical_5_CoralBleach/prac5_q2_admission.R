setwd("/datascience/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_5_CoralBleach/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')

library(ggplot2)
options(digits=10, show.signif.stars = FALSE)

admitData <- read.table("gpa.txt", header=TRUE)
head(admitData)
admitData$rank <- as.factor(admitData$rank)


# plot the data
ggplot(data=admitData, aes(x=gre, y=admit)) + 
      geom_point(shape=19, size=3)

# fit the glm
adm.glm <- glm(admit ~ gre + gpa + rank, data=admitData, family=binomial)
anova(adm.glm, test="Chisq")

# deviance =  499.97652-458.51749, df=5, p=7.5e-8 for final 3-predictor 
# model so (at least one slope diff from zero)
DevianceTest(adm.glm)

# residual deviance for final model = 458.517, df=394, p=value=0.013
# so is large so not good fit. 
ResidualDevianceTest(adm.glm)


# summary
summary(adm.glm) # all coefficients (log odds or odds ratios) are significant 
cof <- cbind(summary(adm.glm)$coef[,1])
cof
100*(exp(cof)-1 )
# INTERPRET: 
# --- gre = when gpa, rank are const, a 1 unit increase in gre score increases
# the odds of being admitted by the UCLA by 0.2267%. 
# --- gpa = when gre, and rank are const, a 1 unit increase in gpa score increases
# the odds of being admitted by the UCLA by 123.4%
# --- rank2: exp(-0.675)=0.509 ===> When gre, gpa are const the odds of being admitted
# to UCLA from a rank2 university is 49.1% less than from a rank1 university.
# --- rank3: when grep, gpa are constant, the odds of being admitted to UCLA
# from a rank3 university is about 73.8% less than from a rank 1 university. 
# --- rank4: when gre, gpa are constant, the odds of being admitted to UCLA
# from a rank4 university is about 78.8% less than from a rank1 university. 

betaCI(adm.glm)
100*(exp(betaCI(adm.glm))-1)
# INTERPRET:
# --- rank2: We are 95% confident that when gre, gpa are const, the odds of being
# admitted from a rank2 university is between 72.7% and 5.5% less than for
# a rank1 university. 
# --- rank3: We are 95% confident that when gre,gpa are const, the odds of
# being admitted from a rank 3 university is between 86.8% and 48.8% less
# than for a rank 1 university. 
# --- rank 4: we are 95% confident that when gre, gpa are const, the odds of being
# admitted from a rank 4 university is between 90.9% and 52.9% less than for
# a rank 1 university. 
# --- gre: we are 95% confdient that for a 1 unit increase in gre score, the
# odds of being addmited rises between 0.0137% and 0.444%. 
# --- gpa: We are 95% confident that for a 1 unit increase in gpa score, the
# odds of being admitted rises between 17.38% and 332.4%. 




# Testing effect of rank - is it significant impact on admissions?
anova(adm.glm, test="Chisq")
# the deviance = 21.826 (last row tests nested between gre+gpa model with
# the gre+gpa+rank model), df = 3, p-value = 0.00007088, so there is a big
# difference between the two models. Can reject teh null hypotehsis
# that the rank slopes are zero. Rank is a significant predictor of admissions.

# OR
adm.main.glm <- glm(admit ~ gre + gpa, data=admitData, family=binomial)
NestedLikelihoodRatioTest(adm.main.glm, adm.glm)





# extra visual
newdata <- data.frame(gre=rep(seq(from=200,to=800, length.out=100), 4*4),
                      gpa=rep(c(2.5, 3, 3.5, 4), each=100*4),
                      rank=factor(rep(rep(1:4, each=100), 4)))

# predict probabilities for input data as well as their stderrors. 
newdata[, c('p', 'se')] <- predict(adm.glm, newdata=newdata,type="response",se.fit=T)[-3]
head(newdata)


ggplot(newdata, aes(x=gre, y=p, colour=rank)) + 
      geom_line(size=1) + facet_wrap(~ gpa)
# each chart is the gpa level, each line is according to rank, the x-axis
# is the gre, and the y-axis is the estimated probability (p)
options(digits=3, show.signif.stars=F)
library(aod)
mydata <- read.table("binary.txt", header=T)

# check for empty cells
xtabs(~ admit + rank, data = mydata)
mydata$rank <- factor(mydata$rank)
# fit logistic regression model
mod.glm <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
anova(mod.glm, test="Chisq")
summary(mod.glm)

mod.glm <- glm(admit ~ gre + gpa + rank, data = mydata, family = "quasibinomial")
anova(mod.glm, test="Chisq")
summary(mod.glm)


## CIs using profiled log-likelihood
confint(mod.glm)
## CIs using standard errors
confint.default(mod.glm)

# Odds ratios R and CIs
exp(cbind(OR = coef(mod.glm), confint(mod.glm)))
# predictions
newdf <- with(mydata,
          data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdf$rank.prob <- predict(mod.glm, newdata = newdf, type = "response")
newdf
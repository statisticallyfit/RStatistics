options(digits=10, show.signif.stars = FALSE)


admitTable <- as.table(matrix(c(1198, 1493, 557, 1278), byrow=TRUE,nrow=2))
colnames(admitTable) <- c("Admitted", "Rejected")
rownames(admitTable) <- c("Male", "Female")
admitTable


oddsRatio(mirror(admitTable))
oddsRatio(admitTable)
# so the odds of being admitted for males is about 84% higher than for females. 




# NOW FOR REGRESSION
admitData <- data.frame(gender=c(rep("Male",6),rep("Female",6)),
                        dept=c("A","B","C","D","E", "F","A","B","C","D","E","F"),
                        admitted=c(512,353,120,138,53,22,89,17,202,131,94,24),
                        rejected=c(313,207,205,279,138,351,19,8,391,244,299,317))
admitData
admitData$total <- admitData$admitted + admitData$rejected
count <- cbind(admitData$admitted, admitData$rejected)

admit.glm <- glm(count ~ gender + dept, data=admitData, family=binomial)
summary(admit.glm)
anova(admit.glm, test="Chisq")

# deviance = 763.402 between gender and gender+dept model so the gender
# variable is significant
# residual deviance = 20.204 p = 0 so the overall model is NOT good fit. 
ResidualDevianceTest(admit.glm)
# deviance = 877.05641 - 20.20428 = 856.8521379
# with df = 11-5=6, p-value = in table = 0 so slopes gender and dept are not zero.
DevianceTest(admit.glm)






# part d) interpret coefs
cof <- summary(admit.glm)$coef # all coefs are significant
cof
exp(cof[,1:2])-1
# SEE THE FILE FOR THIS WORKSHEET IN NOTEBOOK: 
# --- (B0) intercept = the odds of being admitted is 97.7% higher for a female in deptA
# --- (B1) genderMale = odds of being addmitted for a male is 9.5% lower than for
# a female, both in deptA. 
# --- (B2) deptB = odds of being admitted for a female in deptB is 4.2% lower
# than for a female in deptA. 
# --- (B3) deptC = odds of being admitted for a 
# ---
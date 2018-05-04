options(digits=10, show.signif.stars = FALSE)


admitTable <- as.table(matrix(c(1198, 1493, 557, 1278), byrow=TRUE,nrow=2))
colnames(admitTable) <- c("Admitted", "Rejected")
rownames(admitTable) <- c("Male", "Female")
admitTable

# part 1 - analysis by odds ratio. 
oddsRatio(mirror(admitTable))
oddsRatio(admitTable)
oddsMale.admit <- 1198/1493;oddsMale.admit
oddsFem.admit <- 557/1278; oddsFem.admit
oddsMale.admit/oddsFem.admit
# so the odds of being admitted for males is about 84% higher than for females. 
oddsFem.admit/oddsMale.admit
# OR: the odds of being admitted for females is about 0.54 - 1 = 45.7% lower for
# females than for males. 




# part 2 - analysis by proportions
cbind(admitTable, rowProbabilityHat(admitTable)[,1])
# props for males admitted is higher than for females

# but simpson's paradox here since proportions are opposite or nearly the
# same for when the departments are included (not collapsed)
admitData <- data.frame(gender=c(rep("Male",6),rep("Female",6)),
                        dept=rep(c("A","B","C","D","E", "F"),2),
                        admitted=c(512,353,120,138,53,22,89,17,202,131,94,24),
                        rejected=c(313,207,205,279,138,351,19,8,391,244,299,317))
admitData
admitData$total <- admitData$admitted + admitData$rejected
d <- data.frame(AdmitProp=admitData$admitted/admitData$total)
admitData.prop <- data.frame(Dept=LETTERS[1:6], 
                             AdmitProp.Male=d$AdmitProp[1:6],
                             AdmitProp.Female=d$AdmitProp[7:12])
admitData.prop
# see now prop for males is lower. in depts A, B, F. 


# NOW FOR REGRESSION -------------------------------------------------------------

count <- cbind(admitData$admitted, admitData$rejected)

admit.glm <- glm(count ~ gender + dept, data=admitData, family=binomial)
admit.glm2 <- glm(admitted/total ~ gender + dept, data=admitData, weights=total,
                  famil=binomial)
summary(admit.glm)
summary(admit.glm2)
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
# --- (B0) intercept = odds of being admitted for a female in dept A is 97.7%. 
# --- (B1) genderMale = odds of being addmitted for a male is 9.5% lower than for
# a female, both in deptA. (odds ratio of being admitted of male to female in deptA)
# --- (B2) deptB = odds RATIO for females across depts B,A is 4.2% lower
# than the odds RATIO for males across depts B,A. 
# --- (B3) deptC = odds RATIO for females across depts C,A is 71.7% lower
# than the odds RATIO for males across depts C,A. 
# --- (B4) deptD = odds RATIO for females across depts D,A is 72.5% lower
# than the odds RATIO for males across depts D,A. 
# --- (B5) deptE = odds RATIO for females across depts E,A is 82.4% lower
# than the odds RATIO for males across depts E,A. 
# --- (B6) deptF = odds RATIO for females across depts F,A is 96.3% lower
# than the odds RATIO for males across depts F,A.  

# so there is definite gender discrimination - odds are always lower for
# females across all combinations of departments between A and B,C,D,E,F.


ucbFTable <- ftable(UCBAdmissions, col.vars="Admit")
ucbFTable
ucbFTable2 <- ftable(UCBAdmissions, col.vars="Dept")
ucbFTable2


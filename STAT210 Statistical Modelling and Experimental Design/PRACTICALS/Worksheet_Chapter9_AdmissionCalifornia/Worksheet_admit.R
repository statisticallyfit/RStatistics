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

# when combining data by departments, we see bias even goes the 
# opposite directoin (simpson paradox)


# NOW FOR REGRESSION -------------------------------------------------------------

count <- cbind(admitData$admitted, admitData$rejected)

admit.glm <- glm(count ~ gender + dept, data=admitData, family=binomial)
admit.glm2 <- glm(admitted/total ~ gender + dept, data=admitData, weights=total,
                  family=binomial)
summary(admit.glm)
summary(admit.glm2)
anova(admit.glm, test="Chisq") # ORDER OF FIT IMPORTANT

# GENDER: deviance = 763.402 (information in admissions explained by 
# difference in gender) is signif BUT THIS IS AVERAGED OVER departments. 
# DEPT: significant so significant differences in admissions over
# the departments, once gender is fitted.

# ORDER OF FIT MATTERS, gender not signif anymore: 
anova(glm(count ~ dept + gender, data=admitData, family=binomial()))

# FINAL CONCLUSION: to look at summary table since it takes into account
# the fitting of both gender/dept
summary(admit.glm)
# conclude: no difference between males and female admissions across
# the departments (since p = 0.217)
# also: no diff between depts B and A across male and female
# but there are differences for C, D, E, F across male and female. 

#-------------


# residual deviance = 20.204 p = 0 so the overall model is NOT good fit. 
ResidualDevianceTest(admit.glm)
# deviance = 877.05641 - 20.20428 = 856.8521379
# with df = 11-5=6, p-value = in table = 0 so slopes gender and dept are not zero.
DevianceTest(admit.glm)






# part d) interpret coefs
cof <- summary(admit.glm)$coef[,1:2] # all coefs are significant
cof
exp(cof[,1:2])-1
# SEE THE FILE FOR THIS WORKSHEET IN NOTEBOOK: 
# --- (B0) intercept = odds of being admitted for a female in dept A is 97.7%. 
# --- (B1) genderMale = odds of being addmitted for a male is 9.5% lower than for
# a female,  both in dept A,B,C..F (odds ratio of being admitted of male to female 
# in deptA,B,...F)
# --- (B2) deptB = odds of being admitted to dept B is 4.2% lower
# than compared with  dept A. (regardless of gender)
# --- (B3) deptC = odds of being admitted to dept C is 71.7% lower
# than ompared compared with dept A (regardless of gender)
# --- (B4) deptD = odds of being admitted  to dept D is 72.5% lower
# than ompared with dept A. (regardless of gender)
# --- (B5) deptE = odds of being admitted  to dept E is 82.4% lower
# than compared with dept A. (regardless of gender)
# --- (B6) deptF = odds of being admitted to dept F is 96.3% lower
# than ompared with dept A.  (regardless of gender)

# so there is definite gender discrimination - odds are always lower for
# females across all combinations of departments between A and B,C,D,E,F.


# NOTE: to compare odds of being admitted of dept C to dept D, do this
exp(cof[4,1]) / exp(cof[5,1])
# INTERPRET: regardless of gender, odds of being admitted to dept C
# were 1.032 times greater than odds of being admitted to dept D, 
exp(cof[4,1]) / exp(cof[5,1])-1
# or odds of admission to C was 3% higher than for D. 



# -----------------

# INTERACTION analysis
with(admitData, interaction.plot(x.factor=dept, trace.factor=gender, response=admitted/total))

admit.interact.glm <- glm(count ~ gender*dept, data=admitData, family=binomial)

anova(admit.interact.glm, test="Chisq") # saturated model, no df error left
# since df_error = n - k - 1 = 12 - 11 - 1 = 0

summary(admit.interact.glm) # only signif interaction between gender and
# depts B,C, not for the depts D,E,F. 

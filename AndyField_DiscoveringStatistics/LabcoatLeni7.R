# LABCOAT LENI
personalityData <- read.delim("data/Chamorro-Premuzic.dat", header=TRUE)
head(personalityData)
colnames(personalityData) <- c("Age", "Gender", "studentN", 
                               "studentE", "studentO", "studentA", 
                               "studentC", "lecturerN", "lecturerE", 
                               "lecturerO", "lecturerA", "lecturerC")

# Construct the data frames
dropVars <- names(personalityData) %in% 
      c("lecturerE", "lecturerO","lecturerA", "lecturerC")
neuroticLecturer <- personalityData[!dropVars]

dropVars <- names(personalityData) %in% 
      c("lecturerN", "lecturerO","lecturerA", "lecturerC")
extroLecturer <- personalityData[!dropVars]

dropVars <- names(personalityData) %in% 
      c("lecturerE", "lecturerN","lecturerA", "lecturerC")
openLecturer <- personalityData[!dropVars]

dropVars <- names(personalityData) %in% 
      c("lecturerE", "lecturerN", "lecturerO", "lecturerC")
agreeLecturer <- personalityData[!dropVars]

dropVars <- names(personalityData) %in% 
      c("lecturerE", "lecturerN","lecturerA", "lecturerO")
concLecturer <- personalityData[!dropVars]

# Delete missing cases
neuroticLecturer <- neuroticLecturer[complete.cases(neuroticLecturer),]
extroLecturer <- extroLecturer[complete.cases(extroLecturer),]
openLecturer <- openLecturer[complete.cases(openLecturer),]
agreeLecturer <- agreeLecturer[complete.cases(agreeLecturer),]
concLecturer <- concLecturer[complete.cases(concLecturer), ]


# Start the five regressions with two types each

## LECTURER NEUROTICISM --------------------------------------------
n1 <- lm(data=neuroticLecturer, lecturerN ~ Age + Gender)
n2 <- lm(data=neuroticLecturer, 
         lecturerN ~ Age + Gender + studentN + studentE + 
               studentO + studentA + studentC)
summary.lm(n1)
summary.lm(n2)
# significance in change between models
anova(n1, n2)
# To get standardized beta estimates: 
lm.beta(n1) # gender is more important than age
lm.beta(n2) # conscientious student is most important to neurotic lecturer
# Get stats like VIF and dwt
vif(n1)
vif(n2)
dwt(n1) # errors are independent
dwt(n2) # here, too
confint(n1) # gender confint is wider = worse
confint(n2) # intercept confint is widest = worse
# Histogram of residuals
d1 <- data.frame(rstud=rstudent(n1))
ggplot(d1, aes(rstud)) + geom_histogram(fill="dodgerblue")

d2 <- data.frame(rstud=rstudent(n2))
ggplot(d2, aes(rstud)) + geom_histogram(fill="dodgerblue")

# CONCLUDE: age, openness, conscientiousness are significant 
# predictors of wanting a neurotic lecturer (neg for open + conc)


## LECTURER EXTROVERSION --------------------------------------------
e1 <- lm(data=extroLecturer, lecturerE ~ Age + Gender)
e2 <- lm(data=extroLecturer, 
         lecturerE ~ Age + Gender + studentN + studentE + 
               studentO + studentA + studentC)
summary.lm(e1)
summary.lm(e2)
# significance in change between models
anova(e1, e2)
# To get standardized beta estimates: 
lm.beta(e1) # gender is more important than age
lm.beta(e2) # consc + extr student wanted the more extroLecturer
# Get stats like VIF and dwt
vif(e1)
vif(e2)
dwt(e1) # errors are independent
dwt(e2) # here, too
confint(e1) # intercept confint is wider = worse
confint(e2) # intercept confint is widest = worse
# Histogram of residuals
d1 <- data.frame(rstud=rstudent(e1))
ggplot(d1, aes(rstud)) + geom_histogram(fill="dodgerblue")

d2 <- data.frame(rstud=rstudent(e2))
ggplot(d2, aes(rstud)) + geom_histogram(fill="dodgerblue")

# CONCLUDE: conscientious and extroverted students wanted the
# extroverted lecturer most


## LECTURER OPENNESS to experience --------------------------------
o1 <- lm(data=openLecturer, lecturerO ~ Age + Gender)
o2 <- lm(data=openLecturer, 
         lecturerO ~ Age + Gender + studentN + studentE + 
               studentO + studentA + studentC)
summary.lm(o1)
summary.lm(o2)
# significance in change between models
anova(o1, o2)
# To get standardized beta estimates: 
lm.beta(o1) # age is more important than gender
lm.beta(o2) # openness + agreeableness in a positive direction
# Get stats like VIF and dwt
vif(o1)
vif(o2)
dwt(o1) # errors are independent
dwt(o2) # here, too
confint(o1) # intercept confint is wider = worse
confint(o2) # intercept confint is widest = worse
# Histogram of residuals
d1 <- data.frame(rstud=rstudent(o1))
ggplot(d1, aes(rstud)) + geom_histogram(fill="dodgerblue")

d2 <- data.frame(rstud=rstudent(o2))
ggplot(d2, aes(rstud)) + geom_histogram(fill="dodgerblue")



## LECTURER AGREEABLENESS -----------------------------------------
a1 <- lm(data=agreeLecturer, lecturerA ~ Age + Gender)
a2 <- lm(data=agreeLecturer, 
         lecturerA ~ Age + Gender + studentN + studentE + 
               studentO + studentA + studentC)
summary.lm(a1)
summary.lm(a2)
# significance in change between models
anova(a1, a2)
# To get standardized beta estimates: 
lm.beta(a1) # age is more important than gender
lm.beta(a2) # N (+) and Age(-)
# Get stats like VIF and dwt
vif(a1)
vif(a2)
dwt(a1) # errors are independent
dwt(a2) # here, too
confint(a1) # intercept confint is wider = worse
confint(a2) # intercept confint is widest = worse
# Histogram of residuals
d1 <- data.frame(rstud=rstudent(a1))
ggplot(d1, aes(rstud)) + geom_histogram(fill="dodgerblue")

d2 <- data.frame(rstud=rstudent(a2))
ggplot(d2, aes(rstud)) + geom_histogram(fill="dodgerblue")

# CONCLUDE: age and openness to experience had neg.relations with 
# wanting an agreeable lecturer. As neuroticism increases, so does
# desire for agreeable lecturer. 



## LECTURER CONSCIENTIOUSNESS -------------------------------------
c1 <- lm(data=concLecturer, lecturerC ~ Age + Gender)
c2 <- lm(data=concLecturer, 
         lecturerC ~ Age + Gender + studentN + studentE + 
               studentO + studentA + studentC)
summary.lm(c1)
summary.lm(c2)
# significance in change between models
anova(c1, c2)
# To get standardized beta estimates: 
lm.beta(c1) # gender is more important than age
lm.beta(c2) # A (+) and C(+)
# Get stats like VIF and dwt
vif(c1)
vif(c2)
dwt(c1) # errors are independent
dwt(c2) # here, too
confint(c1) # intercept confint is wider = worse
confint(c2) # intercept confint is widest = worse
# Histogram of residuals
d1 <- data.frame(rstud=rstudent(c1))
ggplot(d1, aes(rstud)) + geom_histogram(fill="dodgerblue")

d2 <- data.frame(rstud=rstudent(c2))
ggplot(d2, aes(rstud)) + geom_histogram(fill="dodgerblue")
setwd("/development/projects/statisticallyfit/github/learningmathstat/RStatistics/STAT320 Generalized Linear Models/")
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/FORMULAS.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/PLOTTING.R')
source('/development/projects/statisticallyfit/github/learningmathstat/RStatistics/Rfunctions.R')


library(ggfortify)
library(nlme)
library(lme4)
library(lattice)

options(show.signif.stars = FALSE)


### EXERCISE: FLIES --------------------------------------------------------------

# PROBALEM DESCRIPTION for Fly Data: 
# Three male flies chosen at random is mated with four different
# females chosen at random (so total of 12 females chosen at random). Two offspring
# are born from each mating and the intensity of their eyes is measured (response). 

# Replication = 2 ( means 2 females of each kind within one male kind)

# Observe: (1) the nested part is "female within male" for two reasons: 
# ---> (reason 1) the females are mated with each of the 3 male flies
# ---> (reason 2) the females differ in kinds of females from one male to another. 

# Observe: (2) the factors are both RANDOMLY sampled so they are random effects.

#### MODEL DESCRIPTION for the Analyte Data: 
# --> Variation in (y) eye intensity is a combination of the
# three variances due to: (1) measurements, (2) females, and (3) males 
# --> the three components of variations are due to the distribution of: 
# (1) male means (i) around overall means, 
# (2) female means (j) around the ith male mean, 
# and (3) individual measurements (k) around the jth female mean. 

# Observation: Y_ijk, error_ijk
# male mean = alpha_i
# female mean = beta_j(i)           (female j within male i)

# The model is (for A = Male, B = Female,  both random):

# Y_ijk | Male_i, Female_j(i) ~ Normal(mu_ij | Male_i, Female_j(i), sigma^2)
# mu_ij| Male_i, Female_j(i) = mu + Male_1 + Female_j(i)
#
# error_ijk = Y_ijk - mu_ij
# error_ijk ~ Normal(0, sigma^2)
#
# Female_j(i) ~ Normal(0, sigma_Female ^2)
# Male_i ~ Normal(0, sigma_Male^2)
# 
# cov(Male1, Female_j(i)) = 0
# cov(Male1, error_ijk) = cov(Female_j(i), error_ijk) = 0

# ... so the factors are independent of errors? 


### DIFFERENCE BETWEEN NESTED vs. CROSSED DESIGN: 
# Nested design: DIFFERING females are mated with each of the males. 
# Crossed design: the SAME females would be mated with each of the males. 

flyData <- read.table("data/flies.txt", header=TRUE)
flyData
N <- nrow(flyData)
flyData$female <- factor(paste(rep('F', N), flyData$female, sep=''))
flyData$male <- factor(paste(rep('M', N), flyData$male, sep=''))


 # Exploratory plots

# --- This is NOT the RESIDUAL VARIANCE COMPONENT. ---
# (this is just variatio ndue to females, not really of interest)

### For each female, regardless of male, there is little variability in 
# eye intensity AMONG FEMALES the two offspring because the boxplots are all 
# overlapping, they are not far apart. 
bwplot(eye ~ female, data=flyData)
ggplot(flyData, aes(x=female, y=eye)) + geom_boxplot(size=1, color="hotpink")


# --- This is the MALE VARIANCE COMPONENT.--- 

### Variation among males: If we averaged across females within each male, we  see
# that the average eye intensity differs from male to male. In particular, the mean
# eye intensity of offspring from male 2 would be lower than for male 1, male 3. 
# See that M2 is opposite to M3
bwplot(eye ~ male, data=flyData)
ggplot(flyData, aes(x=male, y=eye)) + geom_boxplot(size=1, color="dodgerblue")

#      geom_boxplot(linetype = "dashed", color = "dodgerblue", size=1) +
#      geom_boxplot(aes(ymin=..lower.., ymax=..upper..), size=1, color="dodgerblue")
  

# --- This is the FEMALE WITHIN MALE VARIANCE COMPONENT. ---

### Variation among females within males: within each male there is large variability
# among eye intesnities from female to fmeale because the boxplots are well-separated
# among females, for all males. 
 
bwplot(eye ~ female | male, layout=c(3,1),data=flyData)

# make fonts bigger to show up on pdf
ggplot(flyData, aes(x=female, y=eye, color=female)) + geom_boxplot(size=1) + 
      facet_grid(. ~ male) + 
      theme(legend.title=element_text(size=20), 
            legend.text=element_text(size=17), 
            plot.title=element_text(size=20), 
            axis.title.x=element_text(size=20),
            axis.title.y=element_text(size=20), axis.text=element_text(size=15))


## RESIDUAL VARIANCE: (variation among replicates): since the boxplots for each female 
# REGARDLESS of male are narrow, there is little variation in eye (response) among
# samples.  Means the replicate measures for each leaf are similar. 

# Interpretation 2: 
# for each female (regardless of male) there is little variability in eye intensity 
# between the two offspring. This will be measured by the residuals variance component)

# (can get same info from dotplots)



# Plot 2

# Ggplot dotplot: 
ggplot(flyData, aes(x=female, color=female, fill=female, y=eye)) + 
  geom_dotplot(binaxis="y", stackdir="centerwhole", stackratio=0.2) + 
  facet_grid(.~male)

dotplot(female ~ eye | male, data=flyData, pch=c(1,1,2,2,3,3,4,4),
        strip=FALSE, strip.left=TRUE, layout=c(1,3), cex=1.5, 
        ylab="female within male", xlab = "eye intensity", jitter.y = TRUE)

# INTERPRET: 
# for each female (regardless of male) there is little variability in eye intensity 
# between the two offspring because the dots (two replicates) are clustered together. 
# This will be measured by the residuals variance component)

# part (c) -----------------------------------------------------------------------
# Fit the mixed effects nested model

# female within male: Female_j(i)
eye.lme <- lme(eye ~ 1, random = ~1|male/female, data=flyData)
summary(eye.lme)

sigma.male <- 4.20786
sigma.femaleWithinMale <- 9.74385
sigma.residual <- 1.1409052

c(sigma.male, sigma.femaleWithinMale, sigma.residual)^2

VarCorr(eye.lme)

# INTERPRET: 
# (1) higher female-within-male variation than male variation and residual variation
# (2) higher within-male variation than residual variation
# +++ Greatest source of variability in eye intensity comes from the females. 


# Proportion of variability in EYE (response) explained by /due to females: 
# (repeatability)
totalVar <- (sigma.male^2 + sigma.femaleWithinMale^2 + sigma.residual^2)
propVarDueToFemales = sigma.femaleWithinMale^2 / totalVar
propVarDueToFemales
# INERPRET: 83.3% of variability in eye intensity of offspring is due to 
# the females. 


# Checking significance of variance components
intervals(eye.lme)
# INTERPRET: 
# sigma.male CI = (0.38, 45.7)
# sigma.female(male) CI = (6.12, 15.5)
# sigma.residual CI = (0.76, 1.70)

# (1) femaleWithinMale Ci is contained within the male CI so the sigma.fmaleWithinMale
# is NOT significantly higher than sigma.male. 
# (2) femaleWithinMale Ci is entirely above the sigma.residual CI so sigma.femWithinMale
# is significantly higher than sigma.residual.
# (3) sigma.residual and sigma.male Ci's are overlapping so these sigmas are not
# significantly different from each other. Within-male variation (sigma.male)
# is NOT significantly higher than sigma.residual. 
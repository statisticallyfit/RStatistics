# WILCOXON RANK SUM TEST (small samples)

" H0: distributions for populations 1 and 2 are identical
  Ha: distributions for pop1 and 2 are different (two tailed)
      or distribution for pop1 lies to left of pop2's (left tailed)
      or distribution for pop1 lies to right of pop2's (right tailed)"

"PROCEDURE: 
1. Rank all n 1 ϩ n 2 observations from small to large.

2. Left tailed test statistic: TL = rank sum of observations in n1.

3. Right tailed test statistic: TR = n1(n1 + n2 + 1) - TL
This is the rank sum of group n1 if the ranks had been reversed from big to small. 

4. Two tailed test statistic: T = min of TR and TL.

5. H0 is rejected if the observed test statistic is beyond the critical
value found using Table 7 in Appendix I.
"

# Example 1

data <- c(169, 178, 180, 180, 182, 185, 188, 190, 225, 235)
species <- c(2,2,2,2,2,2,1,1,1,1)
rank <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
wingstroke <- data.frame(Data=data, Species=species, Rank=rank)
wingstroke

" H0: distributions of wing stroke frequencies are the same for the two species
  Ha: distributions of wing stroke frequencies differ for the two species"

TL <- sum(wingstroke$Rank[wingstroke$Rank >=7])

n1 <- length(wingstroke$Species[wingstroke$Species == 1])
n2 <- length(wingstroke$Species[wingstroke$Species == 2])
TR <- n1*(n1 + n2 + 1) - TL 

Tcrit <- 12 #assuming alpha=0.05, look at Table 7 in mendenhall
Tstatistic <- min(TR, TL)

if(Tstatistic <= Tcrit)
  "Reject null"



# WILCOXON RANK SUM TEST (uses normal approximation)(large samples, n1>= 10, n2>= 10)


" Test statistic: z = (T - n1(n1 + n2 + 1)/2)/sqrt(n1n2(n1 + n2 + 1)/12)"

"EXAMPLE: 

An experiment was conducted to compare the strengths of two types of kraft papers:
one a standard kraft paper of a specified weight and the other the same standard kraft
paper treated with a chemical substance. Ten pieces of each type of paper, randomly
selected from production, produced the strength measurements shown in the table

H0: identical distributions of strengths
Ha: distribution of strength of standard paper is to left of that of treated paper"

standard <- c(1.21, 1.43, 1.35, 1.51, 1.39, 1.17, 1.48, 1.42, 1.29, 1.40)
treated <- c(1.49, 1.37, 1.67, 1.50, 1.31, 1.29, 1.52, 1.37, 1.44, 1.53)
paper <- data.frame(Standard1=standard, Treated2=treated)

"papersorted <- sort(c(standard, treated))
papersorted
which(papersorted==1.29)"
TL = 85.5
TR = 124.5
n1 = n2 = 10
T = min(TL, TR)
#find mu and stdev
muT <- function(n1, n2){return (n1*(n1 + n2 + 1)/2) }
sigmaT <- function(n1, n2){return (sqrt(n1*n2*(n1 + n2 + 1)/12))}
z <- ((T-muT(n1, n2))/sigmaT(n1, n2))
z
# pvalue P(Z <= z)
p.value <- pnorm(z)
p.value
alpha = 0.05
if(p.value < alpha)
  "Reject null"
"Fail to reject null"


# Elementary book: Army and Marines
obstacle.army <- c(15, 18, 16, 17, 13, 22, 24,17,19,21,26,28)
obstacle.marines <- c(14,9,16,19,10,12,11,8,15,18,25)
wilcox.test(obstacle.army, obstacle.marines, correct=F)


# ******************** EXERCISES **********************

"15.1 Suppose you want to use the Wilcoxon rank
sum test to detect a shift in distribution 1 to the right
of distribution 2 based on samples of size n 1 ϭ 6 and
n 2 ϭ 8.
a. Should you use T 1 or T *1 as the test statistic?
b. What is the rejection region for the test if a = 0.05?
c. What is the rejection region for the test if a = 0.01?

15.3 Observations from two random and independent
samples, drawn from populations 1 and 2, are given
here. Use the Wilcoxon rank sum test to determine
whether population 1 is shifted to the left of popula-
tion 2.
Sample 1 1 3 2 3 5
Sample 2 4 7 6 8 6
a. State the null and alternative hypotheses to be
tested.
b. Rank the combined sample from smallest to largest.
Calculate T 1 and T *1 .
c. What is the rejection region for a ϭ .05?
d. Do the data provide sufficient evidence to indicate
that population 1 is shifted to the left of population 2?

15.5 Suppose you wish to detect a shift in 
distribuion 1 to the right of distribution 2 based on sample
sizes n 1 ϭ 12 and n 2 ϭ 14. If T 1 ϭ 193, what do you
conclude? Use a ϭ .05

15.7 
In some tests of healthy,
elderly men, a new drug has restored their memory
almost to that of young people. It will soon be tested
on patients with Alzheimer’s disease, the fatal brain
disorder that destroys the mind. According to Dr. Gary
Lynch of the University of California, Irvine, the drug,
called ampakine CX-516, accelerates signals between
brain cells and appears to significantly sharpen mem-
ory. 2 In a preliminary test on students in their early
20s and on men aged 65–70, the results were particu-
larly striking. After being given mild doses of this
drug, the 65–70-year-old men scored nearly as high as
the young people. The accompanying data are the
numbers of nonsense syllables recalled after 5 minutes
for 10 men in their 20s and 10 men aged 65–70. Use
the Wilcoxon rank sum test to determine whether the
distributions for the number of nonsense syllables
recalled are the same for these two groups.

Alzheimer’s, continued Refer to Exercise
15.6. Suppose that two more groups of 10 men each
are tested on the number of nonsense syllables they
can remember after 5 minutes. However, this time the
65–70-year-olds are given a mild dose of ampakine
CX-516. Do the data provide sufficient evidence to
conclude that this drug improves memory in men aged
65–70 compared with that of 20-year-olds? Use an
appropriate level of a.
20s
65–70s
11 7 6 8 6 9 2 10 3 6
1 9 6 8 7 8 5 7 10 3


15.9 Eye Movement In an investigation
of the visual scanning behavior of deaf children,
measurements of eye movement were taken on nine
deaf and nine hearing children. The table gives the eye-
movement rates and their ranks (in parentheses). Does
it appear that the distributions of eye-movement rates
for deaf children and hearing children differ?
Rank Sum
Deaf Children 
2.75 (15)
2.14 (11)
3.23 (18)
2.07 (10)
2.49 (14)
2.18 (12)
3.16 (17)
2.93 (16)
2.20 (13) 
Hearing Children
.89 (1)
1.43 (7)
1.06 (4)
1.01 (3)
.94 (2)
1.79 (8)
1.12 (5.5)
2.01 (9)
1.12 (5.5)
126 45
"

  



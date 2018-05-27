setwd("/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/PRACTICALS/Practical_7_RatsPlants_Contrasts")

options(show.signif.stars = FALSE)
library(effects)




# Prepare the data. 
pgr <- read.table("pgr.txt", header=TRUE)

plantData <- data.frame(Block=factor(pgr$Block),
                        P=sapply(pgr$P, function(elem){if(elem == 1) "P1" 
                              else if(elem == 2) "P2" else "P3"}),
                        N=sapply(pgr$N, function(e){if(e == 1) "N1" 
                              else if(e == 2) "N2" else if(e == 3) "N3"
                              else if(e == 4) "N4" else "N5"}),
                        stringsAsFactors = TRUE,
                        Yield = pgr$Yield)

# Plotting it 2 ways
attach(plantData)

# by interaction plot
interaction.plot(x.factor = N, trace.factor = P,response=Yield, las=1)
tapply(Yield, INDEX=list(N, P), mean)
# INTERPRET: 
# Change in mean yield from N1 to N2 for both P1 and P3 is nearly the same, but
# different from the lower change in mean yield for P2. Then higher from N2 to N3.
# Higher change for P1 from N3 to N4 and much lower from N4 to N5, while
# the mean yield changes is positive for P2 and negative for P3 from N4 to N5
### So definite interaction in the N3-N4-N5 area. 


# by effects
plant.factorial.lm <- lm(Yield ~ Block + N*P, data=plantData)
eff.pgr <- allEffects(plant.factorial.lm)
plot(eff.pgr)
# INTERPRET: 
# Blocks: no signifc diff in mean yield between blocks 1 and 2
# but there is between 1,2 and Block 3. 
# N*P: similar slopes for N1--N3 for P1,P2,P3
# but different mean changes in Yield from
# N4 to N5 between P1,P2, P3. 



# part d) 
summary(plant.factorial.lm)
# INTERPRET: 
# -- Block1 = mean yield is significantly > 0 for plants in block 1 (intercept)
# -- Block2 = mean yield for block2 plants is not lower than for block1# plants. 
# since p = 1
# -- Block3 = mean yield for block3 plants is same for block1 plants 
# regardless of N and P levels. 
# -- NN2 = mean yield for N2 plants is signif > mean yield for N1 plants
# regardless of block levels and averaged across P levels. 
# -- NN3 = mean yield for N3 plants is signif > mean yield for N1 plants
# regardless of block levels and averaged across P levels. 
# -- NN4 = mean yield for N4 plants is signif > mean yield for N1 plants
# regardless of block levels, and averaged across P levels. 
# -- NN5 = mean yield for N5 plants is signif > mean yield for N1 plants
# regardless of block levels, and averaged across P levels. 

# -- PP2 = mean yield for P2 plants is same as mean yield for P1 plants
# regardless of block levels and averaged across N levels. 
# -- PP3 = mean yield for P3 plants is same as mean yield for P1 plants
# regardless of block levels and averaged across N levels. 

# NN2:PP2 = no difference in mean yield for N=2 and P=2

eff.pgr
tapply(Yield, INDEX=list(N, P), mean)
tapply(Yield, INDEX=Block, mean)


anova(plant.factorial.lm)
# INTERPRET: 
# * no difference in mean yield across the blocks. (p = 0.213), for all P and N levels.
# * significant difference in mean yield across the nitrogen levels (p 4.9e-10), 
# averaged across the plant reg. levels, for all block levels. 
# * significant difference in mean yield across the plant reg. levels, 
# average across the nitrogen levels for all block levels. (p = 0.02)
# * difference in mean yield for any 2 levels of nitrogen depends on the
# levels of plant regulator (P) with p = 0.00009. Interaction is significant.

detach(plantData)
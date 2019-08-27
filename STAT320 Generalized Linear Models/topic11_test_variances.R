# Source topic 11

conc <- rep(c(0.30, 0.30, 0.7,0.6, 1.15, 1.20), 6)
analyteData.smallIQRFarApart <- analyteData
analyteData.smallIQRFarApart$conc <- conc



### Variation among batches WITHIN LABS: 
# Large variability between batches for each lab because the boxplot heights
# for each batch is high among batches. For isntance boxplot for B1 is 
# far apart than for B2, B3.
ggplot(data=analyteData.smallIQRFarApart, aes(x=batch, y=conc, color=batch)) + 
      geom_boxplot() + 
      facet_wrap(. ~ lab, ncol=3)



### Variation among labs: the labs do not differ between each other
# at all since boxplot heights are the same for each lab. 
# This difference/variation between labs distributions mean is measured by 
# the LAB VARIANCE COMPONENT. 

ggplot(data=analyteData.smallIQRFarApart, aes(x=lab, y=conc, color=lab)) + 
      geom_boxplot(size=1)



# Variation among batches avgered over labs: there is llarge variation between 
# individual batches since boxplots are far apart. 

# RESIUDAL VARIANCE COMPONENT. 
ggplot(data=analyteData.smallIQRFarApart, aes(x=batch, y=conc, color=batch)) + 
      geom_boxplot(size=1)




# Fit the nested model: batch within labs: B_j(i)
analyte.smallIQRFarApart.lme <- lme(conc ~ 1, random = ~1|lab/batch, data=analyteData.smallIQRFarApart)
VarCorr(analyte.smallIQRFarApart.lme)


# Results are verified: 

# sigma.lab = 0.00000025005     ( small IQR)
# sigma.batchWithinLab = 0.3688  (far apart)
# sigma.residual (batch avged over lap) = 0.0456



# -------------------------------------------------------------------------------

conc <- rep(c(0.37, 0.20, 0.2,0.4, 0.38, 0.18), 6)
analyteData.largeIQRCloseTogether <- analyteData
analyteData.largeIQRCloseTogether$conc <- conc



### Variation among batches WITHIN LABS: 
# little variability between batches for each lab because the boxplot heights
# for each batch is same among batches. 
ggplot(data=analyteData.largeIQRCloseTogether, aes(x=batch, y=conc, color=batch)) + 
      geom_boxplot() + 
      facet_wrap(. ~ lab, ncol=3)



### Variation among labs: the labs do not differ between each other
# at all since boxplot heights are the same for each lab. 
# This difference/variation between labs distributions mean is measured by 
# the LAB VARIANCE COMPONENT. 

ggplot(data=analyteData.largeIQRCloseTogether, aes(x=lab, y=conc, color=lab)) + 
      geom_boxplot(size=1)



# Variation among batches avgered over labs: there is little variation between 
# individual batches since boxplots are close together in heights apart. 

# RESIUDAL VARIANCE COMPONENT. 
ggplot(data=analyteData.largeIQRCloseTogether, aes(x=batch, y=conc, color=batch)) + 
      geom_boxplot(size=1)




# Fit the nested model: batch within labs: B_j(i)
analyte.largeIQRCloseTogether.lme <- lme(conc ~ 1, random = ~1|lab/batch, data=analyteData.largeIQRCloseTogether)
VarCorr(analyte.largeIQRCloseTogether.lme)


# Results are verified: 

# sigma.lab = 0.00000095     
# sigma.batchWithinLab = 0.000000083  (far apart)
# sigma.residual (batch avged over lap) = 0.096





# -------------------------------------------------------------------------------

conc <- rep(c(0.2, 0.3), c(6,6))
analyteData.largeSigmaSmallBatch <- analyteData
analyteData.largeSigmaSmallBatch$conc <- conc



### Variation among batches WITHIN LABS: 
# little variation among batches within labs since boxplots close
ggplot(data=analyteData.largeSigmaSmallBatch, aes(x=batch, y=conc, color=batch)) + 
      geom_boxplot() + 
      facet_wrap(. ~ lab, ncol=3)



### Variation among labs: 
# large variability among labs (boxplots far apart)
# the LAB VARIANCE COMPONENT. 

ggplot(data=analyteData.largeSigmaSmallBatch, aes(x=lab, y=conc, color=lab)) + 
      geom_boxplot(size=1)



# Variation among batches avgered over labs: there is little variation between 
# individual batches since boxplots are close together in heights apart. 

# RESIUDAL VARIANCE COMPONENT. 
ggplot(data=analyteData.largeSigmaSmallBatch, aes(x=batch, y=conc, color=batch)) + 
      geom_boxplot(size=1)




# Fit the nested model: batch within labs: B_j(i)
analyte.largeSigmaSmallBatch.lme <- lme(conc ~ 1, random = ~1|lab/batch, data=analyteData.largeSigmaSmallBatch)
VarCorr(analyte.largeSigmaSmallBatch.lme)


# Results are verified: 

# sigma.lab = 0.00000095     
# sigma.batchWithinLab = 0.000000083  (far apart)
# sigma.residual (batch avged over lap) = 0.096
library(ggeffects)
library(ggplot2)

data(efc)
head(efc)

efc.lm <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data=efc)

### MARGINAL EFFECTS AT THE MEAN ----------------------------------------
# showing the predicted values for the response, barthtot, at each value
# of the term c12hour (averaged over the other terms in the model)
eff.mean.c12 <- ggpredict(efc.lm , terms="c12hour")
eff.mean.c12
# The terms argument accepts up to 3 model terms where the second and
# third term must indicate grouping levels. 

# Plotting this data
ggplot(eff.mean.c12, aes(x=x, y=predicted)) + geom_line()



### MARGINAL EFFECTS AT THE MEAN FOR DIFFERENT GROUPS --------------------

# Predicting c12hour at different levels of c172code
eff.marginal.c12 <- ggpredict(efc.lm, terms=c("c12hour", "c172code"))
eff.marginal.c12
unique(eff.marginal.c12$group)

ggplot(eff.marginal.c12, aes(x=x, y=predicted, colour=group)) + geom_line()


# Now defining another grouping, which creates another column named facet:
eff.marginal.facet.c12 <- ggpredict(efc.lm, terms=c("c12hour", "c172code", "c161sex"))
eff.marginal.facet.c12
unique(eff.marginal.facet.c12$group) # group = c172
unique(eff.marginal.facet.c12$facet) # facet = c161 (gender)

ggplot(eff.marginal.facet.c12, aes(x=x, predicted, colour=group)) + 
      geom_line() + facet_wrap(~facet)
# seems to be no interaction between c161 / x and x/c172



### AVERAGE  MARGINAL EFFECTS ------------------------------------------------
eff.avg.c12 <- ggaverage(efc.lm, terms=c("c12hour", "c172code"))
eff.avg.c12
ggplot(eff.avg.c12, aes(x, predicted, colour=group)) + geom_line()


## stopped at two and three way interactions: 
# https://strengejacke.wordpress.com/2017/05/24/ggeffects-create-tidy-data-frames-of-marginal-effects-for-ggplot-from-model-outputs-rstats/
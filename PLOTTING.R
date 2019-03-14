#source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ADVANCED_PLOTTING.R')

library(ggplot2)
library(ggfortify) # for residuals plot
library(GGally) # for pairs plot



# influential measures: 
# https://cran.r-project.org/web/packages/olsrr/vignettes/influence_measures.html

# TODO: make cooks plot of F dist with k+1, n-k-1 dfs and show the cooks
# values on that distribution. Mark in red the cutoff point. 





# pairs plot for numerical data 
pairsQuantPlot <- function(data, colsVec, size=10){
      ggpairs(data=data, columns=colsVec, 
              lower=list(continuous="smooth", params=c(colour="blue")),
              diag=list(continuous="density", params=c(colour="black")),
              upper = list(continuous="cor", params=c(size=size)))
}

# For qualitative data ... TODO
# pairsQualPlot


# interaction plot
# TODO: fix so it looks exactly like
# interaction.plot(...) function
interactionPlot <- function(data, xFactor, traceFactor, response){
      ggplot(data=data, aes_string(x=xFactor, y=response, 
                            group=traceFactor, color=traceFactor)) + 
            geom_smooth(method=lm, lwd=1, se=FALSE) + 
            ggtitle(paste("Interaction Plot of",xFactor,"vs.",response,
                          "for Different Levels of",traceFactor)) +
            xlab(xFactor) + ylab(response)
}



# takes single model or block model and plots the effects
effectPlot <- function(fit){
      require(gtable)
      require(effects)
      
      
      
      # Considers the blocking model with 2 qualitative factors and gives the equivalent
      # of plot(allEffects(lm(Yield ~ Location + Herbicide))) for eample. 
      # NOTE: cannot just use singleEffectsPlot twice since the CI estimates
      # from the block model are different than the CI estimates from
      # the separate predictor models. 
      
      #TODO: currently doesn't handle examples like N*P plant growth in prac7
      blockModelEffectsPlot <- function(block.fit) {
            require(gtable)
            require(gridExtra)
            require(effects)
            
            eff <- allEffects(block.fit)
            
            # Now there are only 2 factors so we just have 2 plots
            x1Name <- names(eff)[[1]]
            x2Name <- names(eff)[[2]]
            yName <- eff[[x1Name]]$response
            
            # Make the df of information
            df1 <- data.frame(X1=eff[[x1Name]]$variables[[x1Name]]$levels,
                             Y1=eff[[x1Name]]$fit,
                             lower1=eff[[x1Name]]$lower, upper1=eff[[x1Name]]$upper)
                             
            df2 <- data.frame(X2=eff[[x2Name]]$variables[[x2Name]]$levels,
                             Y2=eff[[x2Name]]$fit,  
                             lower2=eff[[x2Name]]$lower, upper2=eff[[x2Name]]$upper,
                             stringsAsFactors = TRUE)
            
            df1 <- setNames(df1, nm=c(x1Name, yName, "lower1", "upper1"))
            df2 <- setNames(df2, nm=c(x2Name, yName, "lower2", "upper2"))
            
            # Plot the data
            g1 <- ggplot(df1, aes_string(x=x1Name, y=yName)) + 
                  geom_errorbar(aes(ymin=lower1, ymax=upper1), width=0.2,size=1.5,
                                color="palevioletred1") + 
                  geom_line(color="steelblue1", aes(group=1), size=1.5) + 
                  geom_point(size=5, shape=19) + 
                  scale_x_discrete(limits=df1[[x1Name]])+
                  ggtitle(paste(x1Name, "Effects Plot"))
            
            g2 <- ggplot(df2, aes_string(x=x2Name, y=yName)) + 
                  geom_errorbar(aes(ymin=lower2, ymax=upper2), width=0.2,size=1.5,
                                color="palevioletred1") + 
                  geom_line(color="steelblue1", aes(group=1), size=1.5) + 
                  geom_point(size=5, shape=19) + 
                  scale_x_discrete(limits=df2[[x2Name]])+
                  ggtitle(paste(x2Name, "Effects Plot"))
            
            # Now combining the plots into one page
            grid.arrange(g1, g2, nrow=1)
      }
      
      # Plots one single effect for the single qual variable model
      # plot(allEffects(lm(Yield ~ Location)))
      singleEffectPlot <- function(fit){
            
            eff <- allEffects(fit)
            
            # Now there are only 2 factors so we just have 2 plots
            xName <- names(eff)[[1]]
            yName <- eff[[xName]]$response
            
            # Make the df of information
            df <- data.frame(X=eff[[xName]]$variables[[xName]]$levels,
                             Y=eff[[xName]]$fit, 
                             lower=eff[[xName]]$lower, upper=eff[[xName]]$upper,
                             stringsAsFactors = TRUE)
            df <- setNames(df, nm=c(xName, yName, "lower", "upper"))
            
            # Plot the data
            ggplot(df, aes_string(x=xName, y=yName)) + 
                  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2,size=1.5,
                                color="palevioletred1") + 
                  geom_line(color="steelblue1", aes(group=1), size=1.5) + 
                  geom_point(size=5, shape=19) + 
                  scale_x_discrete(limits=df[[xName]]) +
                  ggtitle(paste(xName, "Effects Plot"))
      }
      
      
      # inside effectsPlot()
      eff <- allEffects(fit)
      numPredictors = length(names(eff))
      
      if(numPredictors == 2) blockModelEffectsPlot(fit)
      else if(numPredictors == 1) singleEffectPlot(fit)
      
      # if more than 2 predictors nothing happens. 
}





# residual plot vs fitted (autoplot(fit, which = 1))
residualFitPlot <- function(fit, size=3, colour="black"){
      df <- data.frame(Fits=fit$fitted, Resids=fit$residuals)
      
      ggplot(data=df, aes(x=Fits, y=Resids)) + 
            geom_point(size=size, shape=19, colour=colour) + 
            geom_hline(yintercept=0, size=1, linetype="longdash", colour="red") +
            ggtitle("Residuals vs Fitted") + 
            xlab("Fitted values") + ylab("Residuals")
}


# TODO: normal qq plot for glm
# TODO ggplotify this function so that the red dashed line stands out. Now
# it is currently small and black, unclear in autoplot. 

# Normal qq plot (autoplot(fit, which =2))
normalityPlot <- function(fit, size=2, colour="black"){
      autoplot(fit, which=2, size=size, shape=19, colour=colour)
}

# TODO: autoplot which = 3(scale location plot)

# cooks graph (autoplot which = 4)

# TODO: redoing autoplot residuals stuff (all graphs) without the
# smoothing line!
cooksPlot <- function(fit){
      data <- fit$model 
      data$Obs <- 1:nrow(data)
      data$Cooks <- cooks.distance(fit)
      
      cook.cutOff <- influence.cooksDistances(fit)$Cut[1]
      
      ggplot(data, aes(x=Obs, y=Cooks)) + 
            geom_point(shape=19, size=3) + 
            #geom_histogram(colour="blue", binwidth=1) +
            geom_linerange(ymin=0, ymax=data$Cooks) + 
            geom_hline(yintercept=cook.cutOff, linetype="dashed",size=1,color="red") +
            scale_x_continuous("Observation Number") +
            scale_y_continuous("Cook's distance") +
            ggtitle("Cook's Distance")
}





# Studentized Residuals vs leverage plot (autoplot(lm.fit, which = 5))
studentResidualsLeveragePlot <- function(fit, size=3, colour="purple"){
      df <- data.frame(StudentResid=rstudent(fit), Leverage=hatvalues(fit))
      
      ggplot(data=df, aes(x=Leverage, y = StudentResid)) + 
            geom_point(size=3, shape=19, colour="blue") + 
            geom_hline(yintercept=0, size=2, linetype="longdash", colour="red") + 
            ggtitle("Studentized Residuals vs Leverage") + 
            xlab("Leverage") + ylab("Studentized Residuals")
}



# for multiple regression?
residualsPlot <- function(fit, variableName, size=3, colour="slateblue"){
      xs <- fit$model[[variableName]]
      df <- data.frame(Resids=fit$residuals, Xs=xs)
      
      ggplot(df, aes(x=Xs, y=Resids)) + 
            geom_point(size=size,shape=19,colour=colour) + 
            ggtitle(paste("Residuals vs", variableName)) + 
            xlab(variableName) + ylab("Residuals")
}

# PARTIAL RESIDUAL PLOT FUNCTION (Sincich chapter 8)
# variable = string name
# fit = the lm object

# TODO warning continuous scale when in example8.4 running this line
#partialPlot(coffeeDemandInversePrice.lm, variableName = "I(1/PRICE)")

# TODO make so that interaction variablename is supported - if detects
# colon or is in I(s*s) then must multiply those two somethings to get
# the true xs. 
partialPlot <- function(fit, variableName, size=3, colour="deeppink"){
      # true xs (observed)
      xs <- fit$model[[variableName]]
      # predicted xs (using R's centered method)
      mat <- predict(fit, type="terms")
      centeredFittedXs <- data.frame(mat)
      colnames(centeredFittedXs) <- colnames(mat)
      # now predicted xs just for the variable of interest
      centeredFittedVariable <- centeredFittedXs[[variableName]]
      # calc partial resds : e* = e + bjhat * xj
      partialResidualValues <- fit$residuals + centeredFittedVariable
      
      # making df for ggplot
      df <- data.frame(Partials=partialResidualValues, Xs=xs)
      
      # getting the intercept and slope for the line
      #b0 <- mean(centeredFittedVariable)
      #b.var <- fit$coefficients[[variableName]]
      
      ggplot(df, aes(x=Xs, y=Partials)) + 
            geom_point(shape=20, size=size,color=colour) +
            ggtitle(paste("Partial Residuals of", variableName)) + 
            #geom_abline(intercept=-0.9004155, slope=0.0496918) +
            xlab(variableName) + ylab("Partial Residuals")
}

#da <- data.frame(x=1:10, y=seq(1,20,by=2))
#ggplot(da, aes(x=x,y=y)) + geom_abline(intercept=-1, slope=2) + geom_point()


# Multiplot function defined in cookbook: 
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
      
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      library(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                             ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
            print(plots[[1]])
            
      } else {
            # Set up the page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            
            # Make each plot, in the correct location
            for (i in 1:numPlots) {
                  # Get the i,j matrix positions of the regions that contain this subplot
                  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                  
                  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                  layout.pos.col = matchidx$col))
            }
      }
}






# FIX - must use single x-value even in multiple regression, revert back it the
# way it was!!!


# TODO: make interaction plot with ggplot: groupBy = default arg and etc
# see Example 7.3 Diesel Interaction for the format. 

## Function to use ggplot and add R^2 and so forth
# x.value = the value at which to do meanCI and predicCI intervals
# fit = the model lm fit
modelPlot <- function (fit) {
      
      require(ggplot2)
      #require(ggpmisc) # for statpolyeq function, needs R >= 3.3.3, ggplot 2.2.1
      
      x.names = names(fit$model)[-1]
      y.name = names(fit$model)[1]
      
      R2 = round(summary(fit)$adj.r.squared, 4)
      intercept = round(fit$coef[[1]], 4)
      slopes = round(as.numeric(fit$coef[-1]), 4)
      
      slopesSign = c() #if(slopes < 0) " - " else " + "
      newSlopes = c()
      for (i in 1:length(slopes)) {
            if(slopes[i] < 0){
                  slopesSign = c(slopesSign, " - ")
                  newSlopes = c(newSlopes, abs(slopes[i]))
            }
            else{
                  slopesSign = c(slopesSign, " + ")
                  newSlopes = c(newSlopes, slopes[i])
            }
                  
      }
      # Making the x-names x1 x2 x3 ... etc
      numXs = length(fit$coefficients) - 1
      xStrings = c()
      for(i in 1:numXs){
            xStrings = c(xStrings, paste("x", i, sep=""))
      }
      allCoefsWithVars <- paste(intercept, 
                                paste(c(rbind(slopesSign, newSlopes, xStrings)), 
                                                 collapse = ''), sep="")
      theFormula <- paste("y = ", allCoefsWithVars, sep="")
      #slope = if(slope < 0) abs(slope) else slope 
      #pValue = summary(fit)$coef[2,4]
      #mean.ci = round(meanCI(fit, x.value, level), 4)
      #predict.ci = round(predictCI(fit, x.value, level), 4)
      
      
      ggplot(fit$model, aes_string(x = x.names, y = y.name)) + 
            geom_point(shape=19, size=3) +
            
            #stat_poly_eq(formula = paste(y.name, "~", x.name),
            #             eq.with.lhs = "italic(hat(y))~`=`~",
            #             aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), 
            #             parse = TRUE)
      
            stat_smooth(method = "lm", col="dodgerblue", lwd=1) +
                  
            labs(title = paste("R^2 = ", R2, "\n",
                               "Reg. Line: ", theFormula, #"\n",
                               #" p-value =", pValue,"\n",
                               #"MeanCI = (", mean.ci[1],", ",mean.ci[2],")\n",
                               #"PredictCI = (", predict.ci[1],", ",predict.ci[2],")",
                               sep=""
                               ))
}








plotConfidenceBands.lm <- function(fit) {
      # first check that we have a  single-variable model
      if(length(names(fit$model)) > 2){
            print("Warning: this function only works with a single predictor")
      }
      # Else we get the min-max values and continue
      from <- min(fit$model[2])
      to <- max(fit$model[2])
      xName <- names(fit$model)[2]
      yName <- names(fit$model)[1]
      xs <- data.frame(seq(from=from,to=to, len=nrow(fit$model)))
      colnames(xs) <- xName
      
      CI <- data.frame(predict(fit, newdata=xs, interval="confidence"))
      conf.df <- data.frame(xs[xName], fit = CI$fit, lwr=CI$lwr, upr=CI$upr)
      
      # plotting
      p.data <- ggplot(data=fit$model, aes_string(x=xName, y=yName)) + 
            geom_point(size=3, shape=19)
      
      p.fits = p.data + 
            geom_line(data=conf.df, aes(y=fit, colour="a", linetype="a"),size=1) +
            geom_line(data=conf.df, aes(y=lwr, colour="b", linetype="b"),size=1) + 
            geom_line(data=conf.df, aes(y=upr, colour="b", linetype="b"),size=1) 
      
      p.plot <- p.fits + 
            ggtitle(paste("Predicted and Observed Values of ",yName, " vs ", xName,
                          " and 95% Confidence Bands", sep="")) +
            scale_colour_manual(name="Legend", values=c("a"="red", "b"="dodgerblue"),
                                labels=c("Fitted Line", "95%\nConfidence\nBands")) +
            scale_linetype_manual(name="Legend", values=c("a"="solid", "b"="dashed"),
                                  labels=c("Fitted Line", "95%\nConfidence\nBands")) 
      
      p.plot 
}

# plots both confidence and prediction bands
plotConfPredBands.lm <- function(fit) {
      # first check that we have a  single-variable model
      if(length(names(fit$model)) > 2){
            print("Warning: this function only works with a single predictor")
      }
      # Else we get the min-max values and continue
      from <- min(fit$model[2])
      to <- max(fit$model[2])
      xName <- names(fit$model)[2]
      yName <- names(fit$model)[1]
      xs <- data.frame(seq(from=from,to=to, len=nrow(fit$model)))
      colnames(xs) <- xName
      
      CI <- data.frame(predict(fit, newdata=xs, interval="confidence"))
      PI <- data.frame(predict(fit, newdata=xs, interval="prediction"))
      conf.df <- data.frame(xs[xName], fit = CI$fit, lwr=CI$lwr, upr=CI$upr)
      pred.df <- data.frame(xs[xName], fit = PI$fit, lwr=PI$lwr, upr=PI$upr)
      
      # plotting
      p.data <- ggplot(data=fit$model, aes_string(x=xName, y=yName)) + 
            geom_point(size=3, shape=19)
      
      p.fits = p.data + 
            geom_line(data=conf.df, aes(y=fit, colour="a", linetype="a"),size=1) +
            geom_line(data=conf.df, aes(y=lwr, colour="b", linetype="b"),size=1) + 
            geom_line(data=conf.df, aes(y=upr, colour="b", linetype="b"),size=1) +
            geom_line(data=pred.df, aes(y=lwr, colour="c", linetype="c"),size=1) + 
            geom_line(data=pred.df, aes(y=upr, colour="c", linetype="c"),size=1) 
      
      p.plot <- p.fits + 
            ggtitle(paste("Predicted and Observed Values of ",yName, " vs ", xName,
                          " and 95% Confidence and Prediction Bands", sep="")) +
            scale_colour_manual(name="Legend", values=c("a"="red", "b"="dodgerblue",
                                                        "c"="purple"),
                                labels=c("Fitted Line", "95%\nConfidence\nBands",
                                         "95% Prediction Intervals")) +
            scale_linetype_manual(name="Legend", values=c("a"="solid", "b"="dashed",
                                                          "c"="dotted"),
                                  labels=c("Fitted Line", "95%\nConfidence\nBands",
                                           "95% Prediction Intervals")) 
      
      p.plot 
}

plotConfidenceBands.glm <- function(fit){
      # first check that we have a  single-variable model
      if(length(names(fit$model)) > 2){
            print("Warning: this function only works with a single predictor") 
      }
      
      # Else we get the min-max values and continue
      invLink <- fit$family$linkinv
      from <- min(fit$model[2])
      to <- max(fit$model[2])
      xName <- names(fit$model)[2]
      yName <- names(fit$model)[1]
      xs <- data.frame(seq(from=from,to=to, len=nrow(fit$model)))
      colnames(xs) <- xName
      
      CI <- data.frame(predict(fit, newdata=xs, type="link", se.fit=TRUE))[1:2]
      conf.df <- data.frame(xs[xName], fit = invLink(CI$fit), 
                            lwr = invLink(CI$fit - 2*CI$se.fit), 
                            upr = invLink(CI$fit + 2*CI$se.fit))
      
      # plotting
      p.data <- ggplot(data=fit$model, aes_string(x=xName, y=yName)) + 
            geom_point(size=3, shape=19)
      
      p.fits = p.data + 
            geom_line(data=conf.df, aes(y=fit, colour="a", linetype="a"),size=1) +
            geom_line(data=conf.df, aes(y=lwr, colour="b", linetype="b"),size=1) + 
            geom_line(data=conf.df, aes(y=upr, colour="b", linetype="b"),size=1) 
      
      p.plot <- p.fits + 
            ggtitle(paste("Predicted and Observed Values of ",yName, " vs ", xName,
                          " and 95% Confidence Bands", sep="")) +
            scale_colour_manual(name="Legend", values=c("a"="red", "b"="dodgerblue"),
                                labels=c("Fitted Line", "95%\nConfidence\nBands")) +
            scale_linetype_manual(name="Legend", values=c("a"="solid", "b"="dashed"),
                                  labels=c("Fitted Line", "95%\nConfidence\nBands")) 
      
      p.plot 
}
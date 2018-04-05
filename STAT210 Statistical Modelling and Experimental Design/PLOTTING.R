#source('/datascience/projects/statisticallyfit/github/R/RStatistics/STAT210 Statistical Modelling and Experimental Design/ADVANCED_PLOTTING.R')

library(ggplot2)
library(ggfortify)


# interaction plot
interactionPlot <- function(data, xFactor, traceFactor, response){
      ggplot(data=data, aes_string(x=xFactor, y=response, 
                            group=traceFactor, color=traceFactor)) + 
            geom_smooth(method=lm, lwd=1, se=FALSE) + 
            ggtitle(paste("Interaction Plot of",xFactor,"vs.",response,
                          "for Different Levels of",traceFactor)) +
            xlab(xFactor) + ylab(response)
}


# residual plot
residualFittedPlot <- function(fit, size=4, colour="slateblue"){
      autoplot(fit, which=1, size=size, colour = colour)
}

normalQQPlot <- function(fit, size=4, colour="dodgerblue"){
      autoplot(fit, which=2, size=size, colour=colour)
}

residualPlot <- function(fit, variableName, size=3, colour="slateblue"){
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

da <- data.frame(x=1:10, y=seq(1,20,by=2))
ggplot(da, aes(x=x,y=y)) + geom_abline(intercept=-1, slope=2) + geom_point()


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
ggplotRegression <- function (fit) {
      
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
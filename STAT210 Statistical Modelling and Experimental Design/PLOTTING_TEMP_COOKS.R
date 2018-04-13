# PLOTTING COOKS WITH GGPLOT

#https://www.biostars.org/p/234142/
#https://stackoverflow.com/questions/48962406/add-cooks-distance-levels-to-ggplot2

#p <- length(coef(x))
#usr <- par("usr")
#hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), usr[2L],
#              length.out = 101)
#for(crit in cook.levels) {
#      cl.h <- sqrt(crit*p*(1-hh)/hh)
#      lines(hh, cl.h, lty = 2, col = 2)
#      lines(hh,-cl.h, lty = 2, col = 2)
#}

plotResidLeverageCooks <- function(fit, LEVELS=c(0.2, 0.5, 1), size=2) {
      
      # Functions for calculating the curved Cooks lines. 
      upperContour <- function(leverageList, level, fit) {
            sqrt(level*length(coef(fit))*(1-leverageList)/leverageList)
      }
      lowerContour <- function(leverageList, level, fit) {
            - upperContour(leverageList, level, fit)
      }
      
      # Getting leverages and cooks values
      data <- fortify(fit)
      
      # each contour line per row
      contours.df <- data.frame()
      for(i in 1:length(LEVELS)){
            upMatrix <- upperContour(leverageList=data$.hat, level=lev, fit=fit)
      }
      data$
            ggplot(data=data, aes(x=.hat, y=.stdresid)) + 
            geom_point(size=size, shape=19) +
            geom_hline(yintercept=0, linetype="dashed", colour="gray",size=1) + 
            geom_line()
      
}


plotResidLeverageCooks2 <- function(fit, LEVELS=c(0.5, 1)){
      
      upperContour <- function(leverage, level, fit) {
            sqrt(level*length(coef(fit))*(1-leverage)/leverage)
      }
      lowerContour <- function(leverage, level, fit) {
            - upperContour(leverage, level, fit)
      }
      
      stats <- fortify(fit)
      xLimits <- c(min(stats$.hat), max(stats$.hat))
      yLimits <- c(-3,3)
      
      # from ggfortify
      autoplot(fit, which = 5) +
            stat_function(fun = upperContour, 
                          args = list(level = 0.5, fit = fit), 
                          xlim = xLimits, linetype="longdash", size=1,
                          colour = "red") +
            stat_function(fun = lowerContour, 
                          args = list(level = 0.5, fit = fit), 
                          xlim = xLimits, linetype="longdash", size=1,
                          colour = "red") + 
            scale_y_continuous(limits = yLimits)
}
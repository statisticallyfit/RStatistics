
## Function to use ggplot and add R^2 and so forth
# x.value = the value at which to do meanCI and predicCI intervals
# fit = the model lm fit
ggplotRegression <- function (fit, x.value, level=0.95) {
      
      require(ggplot2)
      #require(ggpmisc) # for statpolyeq function, needs R >= 3.3.3, ggplot 2.2.1
      
      x.name = names(fit$model)[1]
      y.name = names(fit$model)[2]
      
      R2 = round(summary(fit)$adj.r.squared, 4)
      intercept = round(fit$coef[[1]], 4)
      slope = round(fit$coef[[2]], 4)
      slopeSign = if(slope < 0) " - " else " + "
      slope = if(slope < 0) abs(slope) else slope 
      pValue = summary(fit)$coef[2,4]
      mean.ci = round(meanCI(fit, x.value, level), 4)
      predict.ci = round(predictCI(fit, x.value, level), 4)
      
      
      ggplot(fit$model, aes_string(x = x.name, y = y.name)) + 
            geom_point(shape=19, size=3) +
            
            #stat_poly_eq(formula = paste(y.name, "~", x.name),
            #             eq.with.lhs = "italic(hat(y))~`=`~",
            #             aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), 
            #             parse = TRUE)
      
            stat_smooth(method = "lm", col="dodgerblue", lwd=1) +
                  
            labs(title = paste("R^2 = ", R2, "\n",
                               "Reg. Line: y = ", intercept," ",slopeSign," ",slope, "x","\n",
                               " p-value =", pValue,"\n",
                               "MeanCI = (", mean.ci[1],", ",mean.ci[2],")\n",
                               "PredictCI = (", predict.ci[1],", ",predict.ci[2],")",
                               sep=""
                               ))
}


interpret.SlopeCI <- function(x, y, level=0.95, 
                              x.unit="unit", x.name="x", amount.x.unit.increase=1,
                              y.unit="unit", y.name="y", 
                              
                              x.unit.IsPercent=FALSE,
                              y.unit.IsPercent=FALSE)  {
      
      tuple <- slopeCI(x, y, level)
      leftCI <- tuple[1]
      rightCI <- tuple[2]
      
      leftCIValue <- 0
      rightCIValue <- 0 
      if(y.unit.IsPercent){
            leftCIValue <- leftCI * 100
            rightCIValue <- rightCI * 100
      }
      
      #print(amount.x.unit.increase)
      return (cat("There is a ", level*100, " % chance that for each ", amount.x.unit.increase, 
                  " ", x.unit, " increase in x (", x.name, "), the predicted response (", 
                  y.name, ") is expected to increase between ", leftCIValue, " ", 
                  y.unit, " and ", rightCIValue, " ", y.unit, ".", sep="") )
}


# Interpret the meanCI
interpret.MeanCI <- function(x, y, model, x.value, level=0.95,
                             
                             x.unit="unit", x.name="x",
                             y.unit="unit", y.name="y", 
                             
                             x.unit.IsPercent=FALSE,
                             y.unit.IsPercent=FALSE){
      
      tuple <- meanCI(x,y, model, x.value, level)
      leftCI <- tuple[1]
      rightCI <- tuple[2]
      
      leftCIValue <- 0
      rightCIValue <- 0 
      if(y.unit.IsPercent){
            leftCIValue <- leftCI * 100
            rightCIValue <- rightCI * 100
      }
      
      #print(amount.x.unit.increase)
      return(
      cat("There is a ", level*100, " % chance that the interval (", leftCI, ", ", rightCI,
          ") ", y.unit, " encloses the mean ", y.name, " E(y), for all xs (", 
          x.name, ") which had x (", x.name, ") = ", x.value, " ", x.unit, ".", sep=""))
}


# Interpret the predictionCI 
interpret.PredictCI <- function(x, y, model, x.value, level=0.95,
                             
                             x.unit="unit", x.name="x",
                             y.unit="unit", y.name="y", 
                             
                             x.unit.IsPercent=FALSE,
                             y.unit.IsPercent=FALSE){
      
      tuple <- predictCI(x,y, model, x.value, level)
      leftCI <- tuple[1]
      rightCI <- tuple[2]
      
      leftCIValue <- 0
      rightCIValue <- 0 
      if(y.unit.IsPercent){
            leftCIValue <- leftCI * 100
            rightCIValue <- rightCI * 100
      }
      
      #print(amount.x.unit.increase)
      return(cat("With ", level*100, 
                 " % confidence, we predict that the future predicted value of ", 
                 y.name, " in ", y.unit, " when x = ", x.value, " (", x.name, " in ", x.unit, 
                 ") will fall in the interval (", leftCI, ", ", rightCI, ") ", 
                 y.unit, ".", sep=""))
      
}
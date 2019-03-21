library("plyr")
library("dplyr")
library("ggplot2")
library("grid")
library("gridExtra")
library("caret")
library(AppliedPredictiveModeling)
library(RColorBrewer)

data(twoClassData)

twoClass=cbind(as.data.frame(predictors),classes)
twoClassColor <- brewer.pal(3,'Set1')[1:2]
names(twoClassColor) <- c('Class1','Class2')

ggplot(data = twoClass,aes(x = PredictorA, y = PredictorB)) + 
      geom_point(aes(color = classes), size = 2.5) +
      scale_colour_manual(name = 'classes', values = twoClassColor) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0))


### 
nbp <- 250;
PredA <- seq(min(twoClass$PredictorA), max(twoClass$PredictorA), length = nbp)
PredB <- seq(min(twoClass$PredictorB), max(twoClass$PredictorB), length = nbp)
Grid <- expand.grid(PredictorA = PredA, PredictorB = PredB)

PlotGrid <- function(pred,title) {
      
      pts <- (ggplot(data = twoClass, aes(x = PredictorA, y = PredictorB,  
                                          color = classes)) +
                    geom_contour(data = cbind(Grid, classes = pred), aes(z = as.numeric(classes)), 
                                 color = "red", breaks = c(1.5)) +
                    geom_point(size = 4, alpha = .5) + 
                    ggtitle("Decision boundary") +
                    theme(legend.text = element_text(size = 10)) +
                    scale_colour_manual(name = 'classes', values = twoClassColor)) +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0))
      return(pts )
}


V <- 10
T <- 4
TrControl <- trainControl(method = "repeatedcv",
                          number = V,
                          repeats = T)

Seed <- 345
 #####
ErrsCaret <- function(Model, Name) {
      Errs <- data.frame(t(postResample(predict(Model, newdata = twoClass), twoClass[["classes"]])),
                         Resample = "None", model = Name)
      rbind(Errs, data.frame(Model$resample, model = Name))
}

Errs <- data.frame()

#####

CaretLearnAndDisplay <- function (Errs, Name, Formula, Method, ...) {
      set.seed(Seed)
      Model <- train(as.formula(Formula), data = twoClass, method = Method, trControl = TrControl, ...)
      Pred <- predict(Model, newdata = Grid)
      PlotGrid(Pred, Name)
      Errs <- rbind(Errs, ErrsCaret(Model, Name))
}

# LDA PLOT
Errs <- CaretLearnAndDisplay(Errs, "Linear Discrimant Analysis", "classes ~ .", "lda")


# Help cannot get the single plot on the upper right

# http://www.cmap.polytechnique.fr/~lepennec/R/Learning/Learning.html
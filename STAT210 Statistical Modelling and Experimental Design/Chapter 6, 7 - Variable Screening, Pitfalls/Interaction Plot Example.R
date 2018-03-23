# Interaction plot example using effects library from: 
# http://ademos.people.uic.edu/Chapter13.html



library(car) #Even though we already installed "car", we have to tell R we want it to load this package for us to use   
#You can choose whatever # you want for the seed; this is for randomization of your data set
set.seed(150)
#Let's make our data set will have 250 participants (n), perhaps college students!     
n <- 250    
#Uniform distribution of work ethic (X) from 1-5 (1 = poor work ethic, 5 = great work ethic) 
X <- rnorm(n, 2.75, .75)    
#We want a normal distribution of IQ (Z)
#I fixed the mean of IQ to 15 so that the regression equation works realistically, SD = 15 
Z <- rnorm(n, 15, 15)   
#We then create Y using a regression equation (adding a bit of random noise)    
Y <- .7*X + .3*Z + 2.5*X*Z + rnorm(n, sd = 5)
#This code is here so that Y (GPA) is capped at 4.0 (the logical max for GPA)
Y = (Y - min(Y)) / (max(Y) - min(Y))*4
#Finally, we put our data together with the data.frame() function 
GPA.Data <- data.frame(GPA=Y, Work.Ethic=X, IQ=Z)   




# Centering independent variables to avoid multicollinearity
# might be equiv to coding?
GPA.Data$IQ.C <- scale(GPA.Data$IQ, center = TRUE, scale = FALSE)[,]
GPA.Data$Work.Ethic.C <- scale(GPA.Data$Work.Ethic, center = TRUE, scale = FALSE)[,]

GPA.Model.1 <- lm(GPA~IQ.C+Work.Ethic.C, GPA.Data)
GPA.Model.2 <- lm (GPA~IQ.C*Work.Ethic.C, GPA.Data)


# Plotting interactions
library(effects)
#Run the interaction 
Inter.HandPick <- effect('IQ.C*Work.Ethic.C', GPA.Model.2,
                         xlevels=list(IQ.C = c(-15, 0, 15),
                                      Work.Ethic.C = c(-1.1, 0, 1.1)),
                         se=TRUE, confidence.level=.95, typical=mean)

#Put data in data frame 
Inter.HandPick <- as.data.frame(Inter.HandPick)

# note: first row is same as this since it is mean CI
predict(GPA.Model.2, newdata = data.frame(IQ.C=-15, Work.Ethic.C=-1.1), 
        interval="confidence")


#Check out what the "head" (first 6 rows) of your data looks like
head(Inter.HandPick)



# Plot
#Create a factor of the IQ variable used in the interaction                   
Inter.HandPick$IQ <- factor(Inter.HandPick$IQ.C,
                            levels=c(-15, 0, 15),
                            labels=c("1 SD Below Population Mean", "Population Mean", "1 SD Above Population Mean"))

#Create a factor of the Work Ethic variable used in the interaction 
Inter.HandPick$Work.Ethic <- factor(Inter.HandPick$Work.Ethic.C,
                                    levels=c(-1.1, 0, 1.1),
                                    labels=c("Poor Worker", "Average Worker", "Hard Worker"))

library(ggplot2)                
Plot.HandPick<-ggplot(data=Inter.HandPick, aes(x=Work.Ethic, y=fit, group=IQ))+
      geom_line(size=2, aes(color=IQ))+
      ylim(0,4)+
      ylab("GPA")+
      xlab("Work Ethic")+
      ggtitle("Hand Picked Plot")


Plot.HandPick 

options(digits=3, show.signif.stars=F)
salarydf <- read.table("salary.txt",header=T)
salarydf$Gender <- factor(salarydf$Gender,labels=c("M","F") )
# define contrasts
contrasts(salarydf$Gender) <- c(0.5,-0.5)
G.names <- c("M","F")
library(lattice)

# set up lattice graphics (no color, diouble line width)
bwtheme <- standard.theme(color=FALSE)
line<-trellis.par.get("plot.line")
line$lwd=2
trellis.par.set("plot.line", line)
# Exploratory xyplot
xyp.eda1 <- xyplot(Salary ~ YrsEm|Gender,data=salarydf,
                   panel=function(x,y){panel.xyplot(x,y);panel.loess(x,y)})
print(xyp.eda1)

# fit interaction model
model1 <- lm(Salary ~ Gender*YrsEm,data=salarydf,x=T)
anova(model1)
summary(model1)$coefficients
x <- rnorm(50)
mean(x); sd(x); var(x); median(x)

quantile(x)

deciles <- seq(0, 1, 0.10)
quantile(x, deciles)


# analyze juul data + how to skip missing values
library(ISwR)
attach(juul)
mean(igf1, na.rm=TRUE)
sum(!is.na(igf1)) # counts number of nonmissing data
summary(igf1)
summary(juul) # juul is a data frame

# TO rename factor levels
detach(juul)
juul$sex <- factor(juul$sex, labels=c("M", "F"))
juul$menarche <- factor(juul$menarche, labels=c("No", "Yes"))
juul$tanner <- factor(juul$tanner, labels=c("I", "II", "III", "IV", "V"))
attach(juul)
summary(juul)
# OR
juul <- transform(juul, 
                  sex=factor(sex,labels=c("M","F")),
                  menarche=factor(menarche,labels=c("Nope!","Yes")),
                  tanner=factor(tanner,labels=c("I","II","III","IV","V")))
summary(juul)



# Histograms
x <- rnorm(50)
hist(x)

# more control over breaks, by making breaks a vector
mid.age <- c(2.5, 7.5, 13,16.5,17.5,19,22.5,44.5,70.5)
acc.count <- c(28,46,58,20,31,64,149,316,103)
age.acc <- rep(mid.age,acc.count) #rep(values, numtimes)
brk <- c(0,5,10,16,17,18,20,25,60,80)
hist(age.acc, breaks=brk, freq=FALSE)


# Empirical cumulative distribution
x <- rnorm(100)
n <- length(x)
plot(sort(x), (1:n)/n, type="s", ylim=c(0,1)) #OR
plot(ecdf(x))

qqnorm(x)

# boxplot
par(mfrow=c(1,2))
boxplot(IgM)
boxplot(log(IgM))
par(mfrow=c(1,1))


# calculate statistics simultaneously
attach(red.cell.folate)
# tapply
xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
cbind(mean=xbar, std.dev=s, n=n)

tapply(igf1, tanner, mean, na.rm=TRUE)

#aggregate
aggregate(juul[c("age","igf1")], 
          list(gender=juul$sex), sd, na.rm=T) # OR
aggregate(juul[c("age", "igf1")], juul["sex"], mean, na.rm=TRUE)

# by
by = by(juul, juul["sex"], summary)
by$M
by$F



# Graphics for grouped data
attach(energy)
expend.lean <- expend[stature=="lean"]
expend.obese <- expend[stature=="obese"]

par(mfrow=c(2,1))
hist(expend.lean, breaks=10, xlim=c(5, 13), ylim=c(0,4), col="blue")
hist(expend.obese, breaks=10, xlim=c(5, 13), ylim=c(0,4), col="red")
par(mfrow=c(1,1))

# Parallel boxplots
boxplot(expend~stature)

# Stripcharts
opar <- par(mfrow=c(2,2), mex=0.8, mar=c(3,3,2,1)+0.1)
# mex=reduces interline distance, mar=reduces num lines near plot
stripchart(expend ~ stature)
stripchart(expend ~ stature, method="stack")
stripchart(expend ~ stature, method="jitter")
stripchart(expend ~ stature, method="stack", jitter=0.03)
par(opar)
# OR
par(mfrow=c(1,1))
stripchart(list(lean=expend.lean, obese=expend.obese))



# Tables
caff.marital <- matrix(c(652, 1537, 598, 242, 36, 46, 38, 
                         21, 218, 327, 106, 67), 
                       nrow=3, byrow=TRUE)
colnames(caff.marital) <- c("0", "1-150", "151-300", ">300")
rownames(caff.marital) <- c("Married", "Prev.married", "Single")
names(dimnames(caff.marital)) <- c("marital", "consumption")
# convert from matrix to table
caff.marital <- as.table(caff.marital)
caff.marital
# convert from table to data frame
as.data.frame(caff.marital)

table(sex)
table(menarche)
table(sex, menarche)
table(menarche, tanner, sex)
table(tanner, menarche)

xtabs(~ tanner + sex, data=juul) #empty left side can be replaced with pretabulated data
xtabs(~ dgn + diab + coma, data=stroke)

ftable(coma + diab ~ dgn, data=stroke) #flat table

t(caff.marital) #transpose
# aperm transposes multiway tables


# Marginal tables
tanner.sex <- table(tanner, sex)
tanner.sex
margin.table(tanner.sex, 1) # 1 means row total
margin.table(tanner.sex, 2) # 2 means col total

# Relative frequency
prop.table(tanner.sex) * 100 #prop of grand total
prop.table(tanner.sex, 1) * 100 # propoprtion of row
prop.table(tanner.sex, 2) * 100 #proportion of col 
tanner.sex/(sum(tanner.sex)) * 100 # prop of grand total

# Barplots
caff.marital
total.caff <- margin.table(caff.marital, 2) #by col (consumption) addition
total.caff
barplot(total.caff, col="purple")

#if arg for barplot is a matrix, default plot is stacked. Say beside=TRUE
par(mfrow=c(2,2))
barplot(caff.marital, col="blue", beside=TRUE, main="Caff.marital")
barplot(t(caff.marital), col="red", beside=TRUE, main="Transpose")
barplot(prop.table(t(caff.marital), 2), col="pink", beside=TRUE)
barplot(prop.table(caff.marital, 1), col="pink", beside=TRUE)
par(mfrow=c(1,1))

# multi
barplot(prop.table(t(caff.marital), 2), beside=TRUE, 
        legend.text=colnames(caff.marital), 
        col=c("white", "skyblue", "royalblue", "darkblue"))


# Dotcharts
dotchart(t(caff.marital)) #line color lcolor="black" to change

# Piecharts
opar <- par(mfrow=c(2,2), mex=0.8, mar=c(1,1,2,1))
slices <- c("blueviolet", "mediumpurple1", "lavender", "ivory")
pie(caff.marital["Married",], main="Married", col=slices)
pie(caff.marital["Prev.married",], main="Previously married", col=slices)
pie(caff.marital["Single",], main="Single", col=slices)
par(opar)




# ************************** EXERCISES ***********************

"
4.1 Explore the possibilities for different kinds of line and point plots.
Vary the plot symbol, line type, line width, and colour.

4.2 If you make a plot like plot(rnorm(10),type=o) with over-
plotted lines and points, the lines will be visible inside the plotting
symbols. How can this be avoided?

4.3 How can you overlay two qqnorm plots in the same plotting area?
What goes wrong if you try to generate the plot using type=l, and
how do you avoid that?

4.4 Plot a histogram for the react data set. Since these data are highly
discretized, the histogram will be biased. Why? You may want to try
truehist from the MASS package as a replacement.

4.5 Generate a sample vector z of five random numbers from the uni-
form distribution, and plot quantile(z,x) as a function of x (use
curve, for instance).
"
# 1
x <- 1:5; y <- rexp(5, 1); opar <- par(mfrow=c(2,2))
plot(x, y, pch=15)
plot(x, y, type="b", lty="dotted")
plot(x, y, type="b", lwd=3)
plot(x, y, type="o", col="blue")

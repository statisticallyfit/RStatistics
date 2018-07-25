##### Simulations for Example 7.1 ###########
# toss the die 3 times and find the mean of 3 tosses. 
y1 = sample(c(1,2,3,4,5,6), size=1, replace = T) 
y1
y2 = sample(c(1,2,3,4,5,6), size=1, replace = T)
y2
y3 = sample(c(1,2,3,4,5,6), size=1, replace = T)
y3
y_bar = mean(c(y1,y2,y3)) 
y_bar

## Repeat the experiment above with N = 1000 times and oberserve the mean and sd of y_bar 
N=1000;

y1 = rep(0,N) ## create a vector size N with all values are 0 to store the results 
y2 = rep(0,N)
y3 = rep(0,N)
#y4 = rep(0,N)
#y5 = rep(0,N)
#y6 = rep(0,N)
#y7 = rep(0,N)
#y8 = rep(0,N)
#y9 = rep(0,N)
#y10 = rep(0,N)
#y11 = rep(0,N)
#y12 = rep(0,N)

y_bar=rep(0,N)

for (i in 1:N) { 
      y1[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)   
      y2[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
      y3[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
      y_bar[i] = mean(c(y1[i],y2[i],y3[i]))
}
hist(y_bar,ylab = "Frequency",xlab = "Mean of 3 tosses")
summary(y_bar)
mean(y_bar)
sd(y_bar)
var(y_bar)



#y4[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y5[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y6[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y7[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y8[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y9[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y10[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y11[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y12[i] = sample(c(1,2,3,4,5,6), size=1, replace = T)
#y_bar[i] = mean(c(y1[i],y2[i],y3[i],y4[i],y5[i],y6[i],y7[i],y8[i],y9[i],y10[i],y11[i],y12[i]))

loss <- c(372,206,175,154,136,112,55,45,221,166,164,113,82,32,228,196,128,97,64,
          249,219,186,155,114,341,340,283,267,215,148)

par(mfrow=c(1,1))
hist(loss, prob=TRUE, las=1, main="Histogram of rubber loss")

lines(density(loss))
savePlot("hist", "png")

plot(ecdf(loss))

#mode(loss)
median(loss)


betaCI <- function(model){
      beta <- summary(model)$coefficients[,1:2]
      CI <- confint(model)
      beta <- cbind(beta,CI)
      return(beta) }
#______________________________________________
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r1 <- (cor(x, y))
      r<-abs(cor(x,y))
      txt <- format(c(r1, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor * r)
}
#______________________________________________________
panel.corstd <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- (cor(x, y))
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor)
}
#______________________________________________

cor.prob <- function(X, dfr = nrow(X) - 2) {
      R <- cor(X)
      above <- row(R) < col(R)
      r2 <- R[above]^2
      Fstat <- r2 * dfr / (1 - r2)
      R[above] <- 1 - pf(Fstat, 1, dfr)
      R
}

#________________________________________________

makeBiplot <- function(X){
      pc <- princomp(X,cor=T)
      summ <- summary(pc)
      perc <- round(summ$sdev^2/sum(summ$sdev^2)*100,0)
      xlab <- paste("1st Principal component(",perc[1],"%)",sep="")
      ylab <- paste("2nd Principal component(",perc[2],"%)",sep="")
      bp <- (biplot(pc,xlab=xlab,ylab=ylab,cex=1.2))  
      return(bp)
}
#______________________________________________

plot.profiles <- function(x,y){
      n <- length(x)/4
      l1 <- 1:n
      l2 <- (n+1):(2*n)
      l3 <- (2*n+1):(3*n)
      l4 <- (3*n+1):(4*n)
      u <- c(x[l3],rev(x[l4]),x[l3][1])
      v <- c(y[l3],rev(y[l4]),y[l3][1])
      panel.polygon(u,v,col=gray(0.7),border=gray(0.7) )
      panel.xyplot(x[l1],y[l1])
      llines(x[l2],y[l2])  
}   # end of the panel function

#
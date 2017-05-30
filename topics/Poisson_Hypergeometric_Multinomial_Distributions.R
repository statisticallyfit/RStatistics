# Elementary Statistics (8th edition) Exercises

# 3
dmultinom(x=c(2, 2, 2, 2, 2, 2), 
          size=12, 
          prob=c(0.13, 0.13, 0.14, 0.16, 0.20, 0.24))
# 4
dmultinom(x=c(3, 1, 1), size=5, prob=c(0.5, 0.4, 0.1))
# 5
p=1/6
dmultinom(x=c(2, 1, 1), size=4, prob=c(p, p, p))
# 6
dmultinom(x=c(1, 3, 3, 1), size=8, prob=c((9/16), (3/16), (3/16), (1/16)))
# 7
dpois(x=5, lambda=4)
dpois(x=2, lambda=4)
dpois(x=6, lambda=3)
dpois(x=10, lambda=7)
dpois(x=9, lambda=8)
# 8
dpois(x=5, lambda=6)
# 9
lambda = 80000/20000
dpois(0, lambda)
dpois(1, lambda)
dpois(2, lambda)
1-ppois(2, lambda)
# 10
dpois(x=1, lambda=1/2)
# 11
lambda=5/1000*250
1-ppois(q=1, lambda)
ppois(q=1, lambda, lower.tail=F)
# 12
1-ppois(q=1, lambda=1)
# 13
lambda=0.015*200
dpois(x=0, lambda)
# 14
dpois(x=3, lambda=0.03*90)
dbinom(x=3, size=90, prob=0.03)
# 15
dpois(x=5, lambda=4)
# 16
dpois(x=5, lambda=150*8/2000)
dbinom(x=5, size=150, prob=8/2000)
# 17 
choose(5, 1)*choose(4, 1)*choose(5,1)*choose(7,1)/choose(21,4)
# 18
1-(dhyper(x=0, m=5, n=20, k=5) + dhyper(x=1, m=5, n=20, k=5))
1-phyper(1, 5, 20, 5) # same
# 19 
choose(10, 2)*choose(4,1)*choose(2,0)/choose(16,3)
# 20
1-dhyper(x=0, m=6, n=18, k=4)
# 21
1-dhyper(x=0, m=6, n=18, k=3)

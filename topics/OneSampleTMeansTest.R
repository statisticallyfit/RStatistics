# T TEST FOR MEANS

# Substitute teachers
salaries <- c(60, 56, 60, 55, 70, 55, 60, 55)
t.test(salaries, mu=60, alt="less")

pt(2.056, df=10, lower.tail=F)

2*pt(2.983, df=5, lower.tail=F)

# Jogger Oxygen
t.statistic <- (40.6-36.7)/(6/sqrt(15))
pt(t.statistic, df=14, lower.tail=F)

# ******************** EXERCISES **********************

# 6
# alt = less than 2000
avg.acres <- c(959, 1187, 493, 6249, 541)
t.test(avg.acres, mu=2000, alt="less")
# 7
t.stat <- (2.98-2.27)/(0.98/sqrt(20))
t.crit <- qt(0.025, df=19)
2*pt(t.stat, df=19, lower.tail=F)
# 8
t.stat <- (22.1-25.4)/(5.3/5)
t.crit <- qt(0.10, df=24)
pt(t.stat, df=24)
# 9
heights <- c(485,511,841,725,615,520,535,635,616,582)
t.test(heights, mu=700, alt="less")
# 11
tv.times <- c(150,60,90,120,135,150,90,110,200,130)
t.test(tv.times, mu=73, alt="greater")
# 14
cookie.cal <- c(100,100,125,150,150,140,160,135,185,120,
                125,110,155,145,160)
t.test(cookie.cal, mu=110, alt="greater")
# 15
cell.bills <- c(55.83,60.47,58.60,49.88,52.45,51.29,
                62.98,49.2,70.42,50.02)
t.test(cell.bills, mu=50.07, alt="greater")
# 17
woman.visits <- c(3,2,1,3,7,2,9,4,6,6,8,0,5,6,4,2,1,3,4,1)
t.test(woman.visits, mu=5.8)
# 18
job.switches <- c(8,12,15,6,1,9,13,2)
t.test(job.switches, mu=9.2)
# 19
stipends <- c(14000,18000,12000,14356,13185,13419,14000,
              11981,17604,12283,16338,15000)
t.test(stipends, mu=15000)
# 20
family.size <- c(5,4,5,4,4,3,6,4,3,3,5,6,3,3,2,7,4,5,2,2,
                 2,3,5,2)
t.test(family.size, mu=3.18)

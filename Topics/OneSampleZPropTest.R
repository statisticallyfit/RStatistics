
# PROP Z TEST
prop.test(128, 200, 0.6, correct=F)
2*pnorm(((0.64-0.6)/(sqrt(0.6*0.4/200))), lower.tail=F)

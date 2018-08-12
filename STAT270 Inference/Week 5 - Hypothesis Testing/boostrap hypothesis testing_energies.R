# ------ BOOTSTRAP HYPOTHESIS TEST: 

# Trying to test if two samples come from different populations
# H0: F1(x) == F2(y)

N.energy <- c(33,211,284,545,570,591,602,786,945,951,1161,1420,
              1529,1642,1994,2329,2682,2766,3025,13537)
W.energy <- c(269,352,386,1048,1247,1276,1305,1538,2037,2241,2462,2780,
              2890,4081,5358,6498,7542,13791,23862,34734)


# step 1 = get the observed value of t (tobs = x-bar - y-bar)
Z <- c(N.energy, W.energy)
m <- length(N.energy)
n <- length(W.energy)
T.obs <- (mean(W.energy) - mean(N.energy)) / (sd(Z) * sqrt(1 / n + 1/m))
T.obs

# step 2 =  Get a bootstrap sample replicate number
nBS <- 999
T.star <- numeric(nBS)

# step 3 = draw samples of size (m+n) with replacement from the combined data
# which is z = (x,y), which is what we call here Z = (N.energy, W.energy)
# Label the first (m) of these as x* and the remaning (n) as y*. 
for(j in 1:nBS){
      z.star <- sample(Z, size=m+n)
      w.star <- z.star[(m+1) : (m + n)]
      n.star <- z.star[1:m]
      
      # step 4 = calculate t(z*) for each sample. The statistic we are interested in, here
      # we want difference of means.. (?)
      T.star[j] <- (mean(w.star) - mean(n.star)) / ( sd(z.star) * sqrt(1 / m + 1/n))
}

# step 5 = approximating the probability of t_obs or greater by 
# number of t(z*) >= t_obs divided by num bootstrap samples (nBS)
p1 <- sum(T.star >= T.obs) / nBS 

cat( "P(T > ", round(T.obs, 1), "| H0 = true) = ", round(p1, 2), "\n", sep="")

# CONCLUSION; the p-value of 0.03 indicates there is a significant difference
# between the energy means and therefore in their parent population distributions. 
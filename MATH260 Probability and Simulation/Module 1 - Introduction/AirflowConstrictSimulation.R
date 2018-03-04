
# The experiment
# Chronic airflow obstruction (CAO) severely limits the exercise capability of sufferers. Maximum
# exercise ventilation for each of 21 CAO patients was determined under two different experimental
# conditions. Fifteen patients recorded their best ventilation under experimental condition 1.

# The question 
# Of interest is whether the evidence allows us to conclude that either experimental condition gives better results than the other.
# A computing solution We might be tempted to say that clearly experimental condition 1
# is better since 15 of 21 did better. What is wrong with this reasoning? Let us suppose that
# a patient actually performs equally well under either experimental condition. Then it will be
# only a matter of chance under which experimental condition a patient is recorded as performing
# best. The original question can now be restated as: Is it likely that 15 (or more) CAO patients
# will appear to do better under experimental condition 1 (or 2) if in fact there is no difference
# between the effect of the two?

# The simulation 
# Let us tackle the problem experimentally. The problem above is similar to that of tossing a
# coin. Here we could let a head represent a patient doing better under condition 1, a tail as
# better under condition 2. Tossing the coin 21 times then gives us one possible outcome for our
# experiment. If we do this a large number of times then we can observe how many times we get
# 15 or more patients doing better on condition 1 (or condition 2). (Note: If there is really no
#  difference the results could go in either direction, not just the direction observed in the ONE
# experiment described above.) Suppose we carried out the experiment 1000 times, we should be
# able to make some definite conclusion.

# 1 => means patient doing better on condition 1
# 0 => patient doing better on condition 2 
# Let X represent number of times (out of 21) that CAO patients
# under condition 1 do better than under condition 2

sim.results <- numeric(0) # null vector to collect results

for (i in 1:1000) {
      # number of 1's in sample of 21 (0,1)'s
      num.condition1 <- sum(sample(0:1, size=21, replace=TRUE))
      sim.results <- c(sim.results, num.condition1) # accumulate incidences of 1's
}
sim.results
table(sim.results) # frequency of counts out of 21
# table is just counting up frequencies. 
sum(sim.results == 3)
sum(sim.results == 10)

library(ggplot2)

mu_I = 29.87
mu_C = 31.77
sd_I=7.71
sd_C = 7.86
p = 0.957

# part d) pdf of Canandaigua temperature (had muC, varC)
lower <- mu_C - 4 * sd_C
upper <- mu_C + 4 * sd_C
xs <- seq(lower, upper, length=10^3)
ys = dnorm(x=xs, mean=mu_C, sd=sd_C)
df <- data.frame(xs=xs, ys=ys)

ggplot() + 
      geom_line(data=df, aes(x=xs, y=ys), size=1, colour="blue") +
      geom_vline(xintercept=mu_C, colour="black", linetype="dashed", size=1) +
      ggtitle("Normal Density of Canandaigua Max Temperature")



# part e) plot conditiona pdf C | I = 25
mu_C_given_I <- function(i) mu_C + p * (sd_C/sd_I) * (i - mu_I)
sd_C_given_I <- sd_C^2 * (1 - p^2)

df.cond <- data.frame(xs=xs, ys=dnorm(x=xs, mean=mu_C_given_I(25), sd=sd_C_given_I))

ggplot() + 
      geom_line(data=df.cond, aes(x=xs, y=ys), size=1, colour="red") + 
      geom_vline(xintercept=mu_C_given_I(25), colour="black", linetype="dashed",
                 size=1) + 
      ggtitle("Conditional Normal PDF of Canadaigua given Ithaca = 25 degrees F")



# part f) the C|I pdf has smaller variance and smaller mean, so is thinner
# and lower on x-axis
# while the C pdf has bigger variance and mean. 
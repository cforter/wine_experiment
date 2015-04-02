# Standard Deviation  of scores from pilot
standard.dev <- .9

# Vector of potential effect sizes
effects <- seq(.1, 1, by=.01)

#Power calcs for t.test for all possible effect sizes
# We'll use regression, of course, which means 
# our estimate will actually have more power, thanks to covariates
power.dist <- power.t.test(n= 30, delta = effects, sd = standard.dev, sig.level = .05, power = NULL)

#Plots
plot(effects, power.dist$power)
abline(h = .8, col = "red")


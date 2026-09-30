# HYPOTHESIS TESTING: https://www.smajhi.com/DATS-6101/hypothesis-testing/


# The Logic of a Test ----------------------------------------------------------

# 4 Steps:
#   1) State a null hypothesis, H0
#   2) Choose a test statistic
#   3) Work out how that statistic behaves if H0 is true
#   4) Ask how often that behavior would produce something at least as extreme
#     as what you saw. That proportion is the p-value.



# 1) State a null hypohtesis, H0
#     A supplier claims that the mean fill weight is 500 g.

set.seed(8)
n <- 40
claim <- 500
fill <- rnorm(n, mean = 496, sd = 9) # create data to simulate H0
xbar <- mean(fill)
xbar
se <- sd(fill) / sqrt(n)
se

hist(fill, breaks = 20)

# 2) Choose a test statistic, T Statistic
#     We don't know sigma
#     We will estimate sigma by calcuting SE from the sample SD
#     We are making as assumption of normally distributed data

# t = (estimate - claim) / SE
# The difference between the sample estimate and the claim in standardize units

tstat <- (xbar - claim) / se
tstat

round(c(estimate = xbar,
        se = se,
        t = tobs), 4)


# 3) Work out how that statistic behaves if H0 is true

# Assuming the mean = 500 with sample sizes n = 40
# Simulate: mean = 500; sd = 9; n = 40
set.seed(8)
sim_t <- replicate(10000, {
  x <- rnorm(40, mean = 500, sd = 9) # one sample of 40 drawn from a population where H0 is true (mu = 500)
  (mean(x) - 500) / (sd(x) / sqrt(40))
})

hist(sim_t, breaks = 50, freq = FALSE)
curve(dt(x, df = 39), col = "grey50", add = TRUE, lwd = 2)
abline(v = mean(sim_t), col = "red", lwd = 2) # Center of simulated, T0 ~ H0
abline(v = tstat, col = "blue", lwd = 2) # Observed t from sample, TS ~ -2.7








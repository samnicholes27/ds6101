# HYPOTHESIS TESTING: https://www.smajhi.com/DATS-6101/hypothesis-testing/


# The Logic of a Test ----------------------------------------------------------

# 4 Steps:
#   1) State a null hypothesis, H0
#   2) Choose a test statistic
#   3) Work out how that statistic behaves if H0 is true
#   4) Ask how often that behavior would produce something at least as extreme
#     as what you saw. That proportion is the p-value.



# 1) State a null hypothesis, H0 -----------------------------------------------
#     A supplier claims that the mean fill weight is 500 g.

# Simulate data of a sample from the population
set.seed(8)
n <- 40
claim <- 500
fill <- rnorm(n, mean = 496, sd = 9) # sample from population to estimate mu
xbar <- mean(fill)
xbar
se <- sd(fill) / sqrt(n)
se

hist(fill, breaks = 20)

# 2) Choose a test statistic, T Statistic --------------------------------------
#     We don't know sigma
#     We will estimate sigma by calcuting SE from the sample SD
#     We are making as assumption of normally distributed data


# t statistic for sample = (estimate - claim) / SE
# The difference between the sample estimate and the claim in standardized units of the estimate (SE)
tstat <- (xbar - claim) / se
tstat

round(c(estimate = xbar,
        se = se,
        t = tstat), 3)


# 3) Work out how that statistic behaves if H0 is true -------------------------
# What that data look like if H0 is true
# Assuming the mean = 500 with sample sizes n = 40
# Simulate: mean = 500; sd = 9; n = 40
# Where sample (observed) T value values on the H0 distribution
set.seed(8)
sim_t <- replicate(10000, {
  x <- rnorm(n, mean = 500, sd = 9) # one sample of 40 drawn from a population where H0 is true (mu = 500)
  (mean(x) - 500) / (sd(x) / sqrt(40))
})

hist(sim_t, breaks = 50, freq = FALSE,
     main = "Histrogram of t under H0",
     xlab = "Standard Units")
curve(dt(x, df = 39), col = "grey50", add = TRUE, lwd = 2)
abline(v = mean(sim_t), col = "red", lwd = 2) # Center of simulated, T0 ~ H0
abline(v = tstat, col = "blue", lwd = 2) # Observed t from sample, TS ~ -2.7
legend("topright",
       legend = c("Mean of simulated t under H0", "Observed t"),
       col    = c("red", "blue"),
       lwd    = 2,
       bty    = "n")


# 4) As extreme as -------------------------------------------------------------
# Assuming H0 is true how often would you get results at least as extreme as the sample? 

# -abs(tstat) gives us the area to the left of tstat (the lower tail)
# 2 * pt(...) gives us a two-sided test "as extreme as... in either direction

p <- 2 * pt(-abs(tstat), df = n - 1)
round(c(t = tstat, 
        p_value = p), 5)


# Fraction landing at or below −2.70: about 0.0051 (about half a percent)
# Fraction landing at or above +2.70: also about 0.0051, by symmetry
# Two-Sided (total area) = 0.01018

# So p = 0.01018 means: if the true mean were 500 g, about 1 in 100 repeated samples 
# of 40 units would produce a t statistic at least 2.7 standard errors from zero in either direction.


# Using the t.test() function to confirm findings
t.test(fill, mu = 500)


# CONDIIONAL PROBABILITY OF THE P-VALUE ----------------------------------------
################################################################################
# p value = P(data at least this extreme∣ H0 true)
# The probability that the data are at least this extreme assuming the null hypothesis is true
################################################################################


# REJECTING H0 -----------------------------------------------------------------
# Rejecting true H0: Type 1 Error, rate alpha
# alpha is chosen


# Accepting false H0: Type 2 Error, rate beta
# beta is a product of the experiment/study design








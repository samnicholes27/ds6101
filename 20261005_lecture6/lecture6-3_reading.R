

# Power ------------------------------------------------------------------------

# alpha is the false positive rate (chosen)
# alpha = 1 - confidence level

# beta is the false negative rate (inherited by study design)


# Power = 1 - beta
# power = P(rejecting H0 | H0 is false)
# The probability that a study finds a true effect
# The probability of rejecting H0 when H0 is false



set.seed(6101)
# True effect of 0.5 standard deviations. How often does n = 30 detect it?
detected <- replicate(10000, {
  x <- rnorm(30, mean = 0.5, sd = 1)
  t.test(x, mu = 0)$p.value < 0.05
})

round(mean(detected), 3)



# Two Simple Simulations to lead up to Power -----------------------------------
# The simulations assert that xbar is the true mean of the population.
# We show how often random sampling correctly rejects the null value, mean = 0,
# for a one sample t test.The proportion of one sample test that correctly rejects
# the null is the power of the study.

n30 <- 30
xbar <- 0.5
mu <- 0
sd_1 <- 1
sd_d5 <- 0.5
cv <- qt(0.975, n30 - 1)

# Simulation 1
set.seed(6101)
x1 <- rnorm(30, mean = xbar, sd = sd_1) # random sample
t.test(x1, mu = mu) # one sample t test against mu = 0
sim1 <- t.test(x1, mu = mu)
sim1$statistic # t statistic 
cv # critical value
d1 <- (xbar - mu) / sd_1
d1 # cohen's d

round(c(t_stat = sim1$statistic,
        p = sim1$p.value,
        crit_val = cv,
        xbar = mean(x1),
        D = d1), 3)

# Non centrality parameter for a one sample t test
# Assuming d1 is true effect size (H0 is false), the center of the t distribution is given:
t1_center <- d1 * sqrt(n30)
t1_center # noncentral parameter
# if samples of n = 30 were repeatedly drawn, the t-distribution would be centered at t_center

# Simulation 2
set.seed(6101)
x2 <- rnorm(30, mean = xbar, sd = sd_d5)
sim2 <- t.test(x2, mu = mu)
sim2$statistic
cv
d2 <- (xbar - mu) / sd_d5
d2

round(c(t_stat = sim2$statistic,
        p = sim2$p.value,
        crit_val = cv,
        xbar = mean(x2),
        D = d2), 3)

# Non centrality parameter for a one sample t test
# Assuming d2 is true effect size (H0 is false), the center of the t distribution is given:
t2_center <- d2 * sqrt(n30)
t2_center # noncentral parameter
# if samples of n = 30 were repeatedly drawn, the t-distribution would be centered at t_center


# Non centrality parameter for a two-sample t test
# parameter = (mu1 - mu2) / (sigma * sqrt(2/n)) = d * sqrt(n/2)


# Exact, from the noncentral t
power_t <- function(d, n, alpha = 0.05) {
  crit <- qt(1 - alpha/2, df = n - 1)
  ncp  <- d * sqrt(n)
  1 - pt(crit, n - 1, ncp = ncp) + pt(-crit, n - 1, ncp = ncp)
}

power_t(0.5, 30)   # ~0.754
power_t(1.0, 30)   # ~0.9999+





# Replicated Simulations for Sim1 and Sim2
set.seed(6101)
sim1_rep <- replicate(10000, {
  x <- rnorm(n30, mean = xbar, sd = sd_1)
  t.test(x, mu = mu)$p.value < 0.05
})
sim1_power <- round(mean(sim1_rep), 3) # the proportion of replications where the true effect was detected

sim1_tpower <- power.t.test(n = 30, delta = 0.5, sd = 1,   type = "one.sample")$power   # ~0.754

sim1_falseneg <- 1 - sim1_power
sim1_falseneg

# Sim2 increases power by shrinking spread and consequently SE
set.seed(6101)
sim2_rep <- replicate(10000, {
  x <- rnorm(n30, mean = xbar, sd = sd_d5)
  t.test(x, mu = mu)$p.value < 0.05
})
sim2_power <- round(mean(sim2_rep), 3) # the proportion of replications where the true effect was detected

sim2_falseneg <- 1 - sim2_power
sim2_falseneg

# Same setup but using an R function:
power.t.test(n = 30, delta = 0.5, sd = 0.5, type = "one.sample")$power   # ~0.9999+

# Other ways to increase power by study design
# Precise measurement (decrease spread)
# Control variability: homogenous sample, standardized conditions, adjusting for covariates
#   (decrease residual SD)
# Use paired or within-subject design which removes person to person differences


# Increase power by analysis decision
# Increase n
# Choice of alpha



# Power as function of sample size (n) at 3 effect sizes -----------------------
n <- seq(10, 200, by = 5)
pw <- function(d) sapply(n, function(k)
  power.t.test(n = k, delta = d, sd = 1, type = "one.sample")$power)

plot(n, pw(0.5), type = "l", lwd = 2, ylim = c(0, 1),
     xlab = "sample size", ylab = "power",
     main = "Power against n, for three effect sizes")
lines(n, pw(0.3), lwd = 2, col = "steelblue")
lines(n, pw(0.2), lwd = 2, col = "firebrick")
abline(h = 0.8, lty = 2)
legend("bottomright", bty = "n", lwd = 2,
       col = c("black", "steelblue", "firebrick"),
       legend = c("d = 0.5", "d = 0.3", "d = 0.2"))



for (d in c(0.8, 0.5, 0.3, 0.2, 0.1)) {
  n80 <- ceiling(power.t.test(delta = d, sd = 1, power = 0.8,
                              type = "one.sample")$n)
  cat("d =", format(d, nsmall = 1), " n for 80% power:", n80, "\n")
}
# d = 0.8  n for 80% power: 15 
# d = 0.5  n for 80% power: 34 
# d = 0.3  n for 80% power: 90 
# d = 0.2  n for 80% power: 199 
# d = 0.1  n for 80% power: 787 


# Only looking at studies with significant findings:
set.seed(3)
res <- replicate(20000, {
  x  <- rnorm(20, mean = 0.3, sd = 1)
  tt <- t.test(x, mu = 0)
  c(p = tt$p.value, estimate = mean(x))
})

sig <- res["p", ] < 0.05

round(c(true_effect          = 0.3,
        power                = mean(sig),
        mean_estimate_all    = mean(res["estimate", ]),
        mean_estimate_if_sig = mean(res["estimate", sig])), 3)

est <- res["estimate", ]
hist(est, breaks = 60, col = "gray80", border = "white",
     main = "Estimates: all studies vs significant ones", xlab = "Estimated effect") # distribution of all samples
hist(est[sig], breaks = 60, col = rgb(1, 0, 0, 0.5), border = "white", add = TRUE) # distribution of samples with p < 0.05
abline(v = 0.3, lty = 2, lwd = 2, col = "grey5") # mean of all samples
abline(v = mean(res["estimate", sig]), lty = 2, lwd = 2, col = 'darkred') # mean of samples with p < 0.05



# T Distributions of Winner's Curse Simulation ---------------------------------
# Histogram of the null value
# Critical values based on null

# Histogram of sampling and its center
# Significant samples (above cut off) highlighted 

set.seed(3)
t_vals <- replicate(20000, t.test(rnorm(20, mean = 0.3, sd = 1), mu = 0)$statistic)

crit <- qt(0.975, df = 19)     # 2.093
ncp  <- 0.3 * sqrt(20)         # 1.342, center of the t cloud
mean(abs(t_vals) > crit)       # simulated power, ~0.247

# Histogram of the simulated t values
hist(t_vals, breaks = 60, freq = FALSE, col = "gray85", border = "white",
     xlim = c(-4, 8), xlab = "t statistic",
     main = "t values when the true d = 0.3 and n = 20")

# Exact noncentral t curve (the cloud the histogram is estimating)
x <- seq(-4, 8, length.out = 1000)
lines(x, dt(x, df = 19, ncp = ncp), lwd = 2)

# Null t curve for reference (where the cutoffs come from)
lines(x, dt(x, df = 19), lwd = 2, lty = 3, col = "blue")

# Cutoffs
abline(v = c(-crit, crit), lty = 2, col = "red", lwd = 2)

# Center of the cloud
abline(v = ncp, lty = 1, col = "darkgreen", lwd = 2)

# Shade the part of the noncentral curve beyond the upper cutoff (the "wins")
xr <- x[x >= crit]
polygon(c(crit, xr, max(xr)), c(0, dt(xr, 19, ncp = ncp), 0),
        col = rgb(1, 0, 0, 0.4), border = NA)

legend("topright", bty = "n", lwd = 2, lty = c(1, 3, 2, 1),
       col = c("black", "blue", "red", "darkgreen"),
       legend = c("True t cloud (noncentral)", "Null t (H0 true)",
                  "Cutoffs ±2.09", "Center, δ = 1.34"))




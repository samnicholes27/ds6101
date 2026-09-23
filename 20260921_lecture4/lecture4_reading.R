


# Expectation ------------------------------------------------------------------

# expectation is the average outcome of a process you would expect to have in the long-run
# center of mass, the mean for a Random Variable
# where a distribution sits

# E[X] = sum(P(X = x))

# Theoretical Expectation
die <- 1:6 # fair six-sided die
prob <- 1/6 # probability of each side

prob * 6 # check that all probabilities don't exceed 1 (100%)
theoretical_expectation <- sum(die * prob) # expectation = sum of [value of each side x probability of each side]

# Simulated Empirical Expectation
set.seed(6101)
rolls <- sample(1:6, 10000, replace = TRUE) # 1000 random rolls
running <- cumsum(rolls) / seq_along(rolls) # running average after each roll

# snapshot of running average after x number of rolls:
round(c(after_10 = running[10], after_100 = running[100],
        after_1000 = running[1000], after_10000 = running[10000]), 4)

running[10]
running[100]
running[1000]
running[10000]

# average converges on the Expectation over thousands of rolls
plot(seq_along(running), running, type = "l", log = "x", col = "red4",
     xlab = "number of rolls (log scale)", ylab = "running mean",
     ylim = c(2.5, 4.5))
abline(h = 3.5, lwd = 2, lty = 2)


# Variance ---------------------------------------------------------------------

# expectation of the variance of the average in the long-run

# Var(X) = E[X^2] - (E[X])^2
# Var(X) = E[(X - E[X])2]

# Theoretical Variance
# E[X^2]
die <- 1:6
prob <- 1/6
die_sqr <- die^2

expXsq <- sum(die_sqr * prob) # E[X^2]

sqexpX <- theoretical_expectation^2 # (E[X])^2

theoretical_var_exp <- expXsq - sqexpX # Var(X)

theoretical_sd_exp <- sqrt(theoretical_var_exp) # SD(X)


# Simulated Empirical Variance

# Running E[X^2]
running_mean_sq <- cumsum(rolls^2) / seq_along(rolls)

# Var(X)
running_var <- running_mean_sq - running^2

running_sd <- sqrt(running_var)

# Simulation Results
results <- data.frame(
  running_mean = running,
  running_var = running_var,
  runnning_sd = running_sd
)



# Checkpoints
checkpoints <- c(10, 100, 1000, 10000)
round(running[checkpoints], 4)
round(running_var[checkpoints], 4)
round(running_sd[checkpoints], 4)


# Plot : Running Variance
plot(seq_along(rolls), running_var, type = "l", col = "steelblue", lwd = 1.5,
     xlab = "Number of Rolls (n)", ylab = "Running Variance",
     main = "Convergence of Sample Variance to Theoretical Var(X)",
     log = "x")  # log scale on x makes early wobble + late convergence both visible
abline(h = theoretical_var_exp, col = "red", lwd = 2, lty = 2)


# Plot: Running SD
theoretical_sd <- 1.707825
plot(seq_along(rolls), running_sd, type = "l", col = "darkgreen", lwd = 1.5,
     xlab = "Number of Rolls (n)", ylab = "Running SD",
     main = "Convergence of Sample SD to Theoretical SD(X)",
     log = "x")
abline(h = theoretical_sd, col = "red", lwd = 2, lty = 2)



# Linearity --------------------------------------------------------------------



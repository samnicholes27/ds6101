

# Central Limit Theorem --------------------------------------------------------
# Start with a skewed population:
set.seed(6101)
x <- rexp(20000, rate = 1)
hist(x, breaks = 50, col = "gray70", border = "white",
     main = "The population: exponential(1)", xlab = "x")

# How samples of varying sizes affect the distribution of means:
# 4 histograms showing n = 1, 5, 30, 100
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
for (n in c(1, 5, 30, 100)) {
  means <- replicate(20000, mean(rexp(n, rate = 1)))
  hist(means, breaks = 50, col = "gray70", border = "white",
       main = paste("n =", n), xlab = "sample mean")
}



# Skewness ---------------------------------------------------------------------
# Measuring skewness: a measure for asymmetry:
# skewness = 0, normal distribution
# skewness > 0, right tailed skewness
# skewness < 0, left tailed skewness


# Define function to calculate skewness
skew <- function(v) mean((v - mean(v))^3) / sd(v)^3

# Right Tailed Skewness
out <- t(sapply(c(1, 5, 30, 100), function(n) {
  m <- replicate(20000, mean(rexp(n, rate = 1)))
  c(n = n, mean = mean(m), sd = sd(m), predicted_sd = 1 / sqrt(n), skew = skew(m))
}))
round(out, 4)

# Left Tailed Skewness
out <- t(sapply(c(1, 5, 30, 100), function(n) {
  m <- replicate(20000, mean(-rexp(n, rate = 1)))   # the only change: the minus sign
  c(n = n, mean = mean(m), sd = sd(m), predicted_sd = 1 / sqrt(n), skew = skew(m))
}))
round(out, 4)


# A log-normal, of the kind you meet in revenue and duration data.
pop_skew <- skew(rlnorm(200000, meanlog = 0, sdlog = 1.2))

res <- t(sapply(c(30, 100, 1000), function(n) {
  c(n = n, skew_of_means = skew(replicate(8000, mean(rlnorm(n, 0, 1.2)))))
}))
round(rbind(c(n = 1, skew_of_means = pop_skew), res), 3)



# Confidence Intervals ---------------------------------------------------------

# A sample of 200 commute times, from an example population 
commute <- rnorm(200, mean = 31, sd = 14)

xbar <- mean(commute)
se   <- sd(commute) / sqrt(length(commute))

round(c(estimate = xbar, 
        se = se,
        lower = xbar - 1.96 * se, 
        upper = xbar + 1.96 * se), 
      3) # rounded to 3 decimals



# Coverage of CI
# 95% means that if we had 100 samples, the true population would be found in 95 of them.
# 5 would not contain the population parameter


mu <- 31        # the population parameter, mu (mean)
reps <- 100

ints <- t(replicate(reps, {
  x    <- rnorm(40, mean = mu, sd = 14)
  se   <- sd(x) / sqrt(40)
  mean(x) + c(-1, 1) * qt(0.975, df = 39) * se
}))

covers <- ints[, 1] <= mu & mu <= ints[, 2]
cat("intervals containing the truth:", sum(covers), "out of", reps, "\n")

plot(NA, xlim = range(ints), ylim = c(0, reps), xlab = "commute time",
     ylab = "study number", main = "100 studies, 100 intervals")
segments(ints[, 1], 1:reps, ints[, 2], 1:reps,
         col = ifelse(covers, "gray60", "firebrick"), lwd = 2)
abline(v = mu, lwd = 2)


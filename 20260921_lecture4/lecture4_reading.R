


# The Normal Distribution ------------------------------------------------------

# Parameters:
#   μ, mu - center (mean)
#   σ, sigma - spread (standard deviation)
# The mean and standard deviation are independent of each other,
# you can have any center with any spread.
#
# The mean and standard deviation are finite. Meaning, the mean is a specific number.
# Finite standard deviation describes how fast the tails of the distribution decay.

# Central Limit Theorem
# For sample of independent observations (n) drawn from a population, the means of
# each sample will converge upon a distribution (Normal distribution), with a mean
# and standard deviation. The key to this pattern is dependent on n grows larger.


# Empirical Rule: 68-95-99.7
# 68% of data falls within 1 SD of the mean
# 95% of the data falls within 2 SDs of the mean
# 99.7% of the data falls within 3 SDs of the mean


# Simulation, Normal Distribtion  ----------------------------------------------

set.seed(27)

# Plot to demonstrate how mu affects position or the center of distribution
par(mfrow = c(1, 1))
x <- seq(-10, 20, length.out = 500)
plot(x, dnorm(x, mean = 0, sd = 2), type = "l", lwd = 2, col = "black",
     ylab = "density", main = "Same sigma, different mu")
lines(x, dnorm(x, mean = 5, sd = 2), lwd = 2, col = "blue")
lines(x, dnorm(x, mean = 10, sd = 2), lwd = 2, col = "red")
legend("topright", legend = c("mu=0", "mu=5", "mu=10"),
       col = c("black","blue","red"), lwd = 2)

# Plot to demonstrate how sigma affects the spread of the distribution
plot(x, dnorm(x, mean = 5, sd = 1), type = "l", lwd = 2, col = "black",
     ylab = "density", main = "Same mu, different sigma")
lines(x, dnorm(x, mean = 5, sd = 2), lwd = 2, col = "blue")
lines(x, dnorm(x, mean = 5, sd = 4), lwd = 2, col = "red")
legend("topright", legend = c("sigma=1", "sigma=2", "sigma=4"),
       col = c("black","blue","red"), lwd = 2)


# Independence of mu and sigma
big_mean_small_sd  <- rnorm(10000, mean = 100, sd = 1)
small_mean_big_sd  <- rnorm(10000, mean = 1,   sd = 20)

c(mean(big_mean_small_sd), sd(big_mean_small_sd))
c(mean(small_mean_big_sd), sd(small_mean_big_sd))


# Empiral Rule
z <- rnorm(100000, mean = 0, sd = 1)
mean(abs(z) < 1)   # within 1 sd
mean(abs(z) < 2)   # within 2 sd
mean(abs(z) < 3)   # within 3 sd


# Simulation, Central Limit Theorem --------------------------------------------


# Population
# Population is intentionally skewed to be able to see how the CTL works 
# (distribution of sample means is normal)

set.seed(21)

population <- rexp(100000, rate = 1)
hist(population,
     breaks = 50, col = "grey70", border = "white",
     main = "Population (skewed)",
     xlab = "")
abline(v = mean(population), col = "red", lwd = 2, lty = 1)
abline(v = median(population), col = "blue", lwd = 2, lty = 1)

mean(population)
median(population)
sd(population)

mean(population) - median(population)




# Small sample size
set.seed(22)
small_sample <- sample(population, size = 5, replace = TRUE)
hist(small_sample,
     breaks = 5, col = "grey70", border = "white", replace = TRUE,
     main = "Small Sample",
     xlab = "")


small_mean <- replicate(10000, mean(sample(population, size = 5, replace = TRUE)))
hist(small_mean,
     breaks = 50, col = "grey70", border = "white",
     main = "Small Sample",
     xlab = "")
abline(v = mean(small_mean), col = "red", lwd = 2, lty = 1)
abline(v = median(small_mean), col = "blue", lwd = 2, lty = 1)

mean(small_mean)
median(small_mean)
sd(small_mean)

mean(small_mean) - median(small_mean)


# Large sample size
set.seed(23)

large_sample <- sample(population, size = 50, replace = TRUE)
hist(large_sample,
     breaks = 50, col = "grey70", border = "white",
     main = "Large Sample",
     xlab = "")


large_mean <- replicate(10000, mean(sample(population, size = 50, replace = TRUE)))
hist(large_mean,
     breaks = 50, col = "grey70", border = "white",
     main = "Large Sample",
     xlab = "")
abline(v = mean(large_mean), col = "red", lwd = 2, lty = 1)
abline(v = median(large_mean), col = "blue", lwd = 2, lty = 1)

mean(large_mean)
median(large_mean)
sd(large_mean)

mean(large_mean) - median(large_mean)









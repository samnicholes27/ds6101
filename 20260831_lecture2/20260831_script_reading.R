
set.seed(6101) # seed
salary <- round(rlnorm(240, meanlog = 11, sdlog = 0.55)) # create a log normal distribution of salaries


head(salary)
length(salary)


plot(salary)

# MEASURES OF CENTER -----------------------------------------------------------
# mean > median - right skewed distribution
mean(salary)
median(salary)

# Histogram with plot lines for mean and median
hist(salary,
     breaks = 30,
     col = "gray85",
     border = "white",
     main = "",
     xlab = "Annual Salary (USD)")
abline(v = mean(salary), 
       lwd = 3, lty =1)
abline(v = median(salary), 
       lwd = 3, lty = 2)

sum(salary < mean(salary))
# salary <- mean(salary) results in logical vector: TRUE | FALSE
# sum() coerces to 1s | 0s and adds the values resulting in the count of 1s

length(salary < mean(salary)) # gives the length of the logical vector

mean(salary < mean(salary))
# mean() coerces boolean values to binary, 1 | 0
# 1s add up to 145, which gives 0.60 when divided by the total 
# and gives the percentage of 1s in the logical vector



# MEASURES OF SPREAD -----------------------------------------------------------

var(salary) # not intuitive way to measure spread of data
sd(salary) # more intuitive/interpretable way to measure spread of data
mean(salary) + sd(salary) # $110,978.70 is 1SD away from the mean


# Exercise: the correction, by hand
# The vector `s` holds five values. R's var(s) is 16 (sd = 4).
# Compute the OTHER version: the average squared deviation from the mean,
# dividing by n rather than n - 1. Write it out; do not use var().
n <- 5
s <- rnorm(5, mean = 0, sd = 4)

# Scenario A: s is a sample from population, p
var_s <- sum((s - mean(s))^2) / (n - 1)
var_s

# Scenario B: s is the entire known population
var_s <- sum((s - mean(s))^2) / (n)
var_s


# Exercise: below average
# `salary` holds the 240 alumni salaries.
# Return the PROPORTION of alumni who earn less than the mean salary
# (a single number between 0 and 1).
salary <- round(rlnorm(240, meanlog = 11, sdlog = 0.55)) 
mean_salary <- mean(salary)
alumni <- mean(salary < mean(salary))
alumni


# Interquartile Range (IQR)
quantile(salary,
         probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

# IQR/SD gives an idea of how data are spread out from the center
# IQR/SD = 1.35 - normal distribution
# IQR/SD < 1.35 - heavy tails (outliers, bulk of data is less centralized)
# IQR/SD > SD - lighter tails (data is more heavily clustered around the center)
IQR(salary)/sd(salary)
# 1.25
# 1.25 < 1.35 - heavy tailed distribution



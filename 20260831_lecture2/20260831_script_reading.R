
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


# OUTLIERS ---------------------------------------------------------------------
# Identify a potential outlier (a datum that is far away) if it is 1.5x IQR
# Better to look at this check as identifying data that is far away\

quantile(salary, 0.75) # Q3 = 91671.25
IQR(salary) # IQR = 49378.75
1.5 * IQR(salary) # 1.5x IQR = 74068.12

# set upper bound to determine 1.5x IQR
upper_bound <- quantile(salary, 0.75) + 1.5 * IQR(salary)
upper_bound # 165739.4, if a datum is beyond this, it is a potential outlier
sum(salary > upper_bound) # 6 data are beyond the upper bound



# VISUALIZATIONS ---------------------------------------------------------------
# Create data
grp <- sample(c(0, 1), 400, replace = TRUE)
commute <- rnorm(400, mean = ifelse(grp == 1, 72, 34), sd = 6)
summary(commute)
sd(commute)


# Histogram
# Histograms show shape of data, but hide outliers/tails depending on bin width and bar height
hist(commute, breaks = 30, col = "gray85", border = "white",
     main = "", xlab = "one-way commute (minutes)")

# Box plot
# boxplots show outliers, but hide shape (can't tell that distribution is bimodal)
boxplot(commute, horizontal = TRUE, col = "gray85",
        xlab = "one-way commute (minutes)")


# Using density() to smooth a histogram
hist(commute, breaks = 30, freq = FALSE, col = "gray90", border = "white",
     main = "", xlab = "one-way commute (minutes)")
lines(density(commute), lwd = 3)



# Summary/Descriptive Stats Function -------------------------------------------
set.seed(11)
n <- 200
staff <- data.frame(
  id     = 1:n,
  dept   = sample(c("eng", "ops", "sales"), n, replace = TRUE),
  salary = round(rlnorm(n, 11.2, 0.35)),
  years  = sample(0:30, n, replace = TRUE)
)
staff$salary[sample(n, 24)] <- NA
staff$rating <- NA_real_          # a column somebody added and never filled

# profile() function to generate summary and descriptive stats
profile <- function(df) {
  data.frame(
    type    = sapply(df, function(x) class(x)[1]),
    missing = sapply(df, function(x) sum(is.na(x))),
    unique  = sapply(df, function(x) length(unique(x[!is.na(x)]))),
    min     = sapply(df, function(x) if (is.numeric(x)) min(x, na.rm = TRUE) else NA),
    median  = sapply(df, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else NA),
    max     = sapply(df, function(x) if (is.numeric(x)) max(x, na.rm = TRUE) else NA)
  )
}

profile(staff) # call function












set.seed(6101) # seed
salary <- round(rlnorm(240, meanlog = 11, sdlog = 0.55)) # create a log normal distribution of salaries


head(salary)
length(salary)


plot(salary)

# Measures of Center:
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
# salary < mean(salary) results in logical vector: TRUE | FALSE

length(salary < mean(salary)) # gives the length of the logical vector
mean(salary < mean(salary)) 




# sum(), mean() coerces logical values into 1 | 0 
# sum() results in summation of binary values
#




# P-Values when the null is true -----------------------------------------------

set.seed(8)
# 20,000 studies in a world where the null is exactly true (does not deviate from mu = 0)
p <- replicate(20000, t.test(rnorm(30), mu = 0)$p.value) # 20000 p values

hist(p, breaks = 40, col = "gray75", border = "white",
     main = "p-values when the null is TRUE", xlab = "p")

round(c(P_below_0.05 = mean(p < 0.05), # 5% of p values are found up to p = 0.05
        P_below_0.50 = mean(p < 0.50), # 50% of p values are found up to p = 0.50
        P_below_0.90 = mean(p < 0.90)), 4) # 90% of p values are found up to p = 0.90

# No data from sampling come as a surprise - what would be a rare occurrence under the null is
# not rare because the null is true.

# Null true: p-values are flat, so p < 0.05 happens 5% of the time (false positives)





# P-Values when the null is false ----------------------------------------------

set.seed(8)
# Null is FALSE: true mean is 0.5, but we still test against mu = 0
# 20000 p values when the null is false
p_alt <- replicate(20000, t.test(rnorm(30, mean = 0.5), mu = 0)$p.value)

hist(p_alt, breaks = 40, col = "gray75", border = "white",
     main = "p-values when the null is FALSE (true mean = 0.5)", xlab = "p")

round(c(P_below_0.05 = mean(p_alt < 0.05), # 75% of p-values are found up to p = 0.05
        P_below_0.50 = mean(p_alt < 0.50), # 97% of p-values are found up to p = 0.50
        P_below_0.90 = mean(p_alt < 0.90)), 4) # 99# of p-values are found up to p = 0.90

# the bulk of p-values sit below p = 0.05. When the null is false, it's common to find data that don't match the null
# Large p-values are rare (data that align with a false null)

# Null false: p-values pile up near 0, so p < 0.05 happens often, 
# and p > 0.05 happens less often (false negatives).




# WHERE P-VALUES FALL ALONG THE DISTRIBTUION  ----------------------------------
################################################################################
# Large p-values (0.8, 0.9) mean the data sit close to what the null predicts, with the sample mean near the claimed value. 
# Small p-values (below 0.05) mean the data sit far from what the null predicts, in the extreme tails of the null world
################################################################################




# How P-Values relate to T-Values when H0 is True ------------------------------
set.seed(8)
t_vals <- replicate(20000, t.test(rnorm(30), mu = 0)$statistic) # 20000 t values
p_vals <- 2 * pt(-abs(t_vals), df = 29) # p values for each t value

par(mfrow = c(1, 2))
hist(t_vals, breaks = 40, main = "t statistics (bell-shaped)", xlab = "t")
hist(p_vals, breaks = 40, main = "p-values (flat)", xlab = "p")



# A classroom analogy to explain p distribution flatness

# Say 100 students take an exam, and you label each by class standing:
#  1st, 2nd, ..., 100th. Now ask, "what is the probability that a randomly chosen 
# student has standing between 41st and 50th?" It's 10%. "Between 91st and 100th?" 
# Also 10%. The standings are not "likely" or "unlikely" on their own. They are just 
# evenly handed out. The exam scores might clump around 75, but that clumping doesn't 
# change how standings are distributed.

# Replace "exam score" with "t statistic" and "class standing" with "p-value," 
nd you have the same situation.




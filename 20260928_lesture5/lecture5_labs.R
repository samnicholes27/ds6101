
"""

Build 100 intervals from 100 fresh samples of 30. Count how many contain 100.

Predict the count first.

"""


# Prediction: ______

set.seed(6101)
covered <- ______

covered


replicates <- 100



set.seed(6101) # 

mu <- 100
n <- 30

covered <- replicate(100, {
  x  <- rnorm(n, mean = mu)
  ci <- mean(x) + c(-1, 1) * qt(0.975, n - 1) * sd(x) / sqrt(n)
  ci[1] <= mu && mu <= ci[2]
})

round(mean(covered), 4)








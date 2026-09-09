
# SIMULATION: The Medical Test Problem -----------------------------------------
set.seed(6101)


N <- 100000 # Total population
diseased <- rbinom(N, 1, 0.005) # 0.5% of the population is diseased

# Sensitivity = 99%, 99 / 100 who test with disease will correctly test positive
# Specificity = 95%, 5% false positive
positive_test <- rbinom(N, 1, prob = ifelse(diseased == 1, 0.99, 0.05)) # total who test positive

table(Disease = diseased,
      Positive = positive_test)

mean(diseased[positive_test == 1])
# 0.09134874
# P(D|+)
# Positive Predictive Value, PPV)

# Baye's Rule
prev <- 0.005 # P(D)
sens <- 0.99 # P(+|D)
spec <- 0.95 # P(-|ND)
ppv <- sens * prev / (sens * prev + (1 - spec) * (1 - prev))
round(ppv, 4)

rbind(
  "has disease"      = c(positive = 100000 * prev * sens,
                         negative = 100000 * prev * (1 - sens)),
  "does not have it" = c(positive = 100000 * (1 - prev) * (1 - spec),
                         negative = 100000 * (1 - prev) * spec)
)




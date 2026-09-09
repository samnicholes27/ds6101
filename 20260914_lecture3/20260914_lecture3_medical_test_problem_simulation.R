
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


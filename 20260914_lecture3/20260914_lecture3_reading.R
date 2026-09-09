

# PROBABILITY RULES ------------------------------------------------------------

# P(A ∩ B) = P(B) * P(A|B), Multiplication Rule
# Where A and B are both true simultaneously

# P(A|B) = P(A ∩ B) / P(B)
# Where P(B) > 0


# P(A|B) = [P(B|A) * P(A)] / P(B)
# Baye's Rule allows use to calculate P(B|A) or P(A|B)

# Prior = P(A), What you believe before seeing evidence from P(B)
# Posterior = P(A|B), updated belief about A after incorporating evidence from P(B)

# Base-rate neglect is confusing P(A|B) with P(B|A)

# Read structurally: you start with the prior P(A), 
# multiply it by how well the evidence discriminates 
# (the likelihood, P(B|A)), and divide by the overall 
# probability of seeing that evidence at all (P(B), 
# summed across both cases where A is true and where A is false). 
# The output is the posterior.

# One useful way to hold the pair conceptually: 
# the posterior from one round of evidence can become 
# the prior for the next round, if more evidence comes in. 
# If a second, independent piece of evidence B₂ came in, 
# you wouldn't restart at P(A) — you'd use P(A | B) 
# as your new prior and update again. Bayesian updating 
# is sequential in that sense; "posterior" and "prior" aren't 
# fixed labels for particular numbers, they're roles relative 
# to a specific piece of evidence.






# EXAMPLE ----------------------------------------------------------------------
# A disease affects 0.5% of the population; P(D) = 0.005
# Total population is 1,000
# Sensitivity = 99%; Out of the truly diseased, how many correctly test positive?
#   P(+|D)

# Specificity = 95%; Out of the NON-diseased, how many correctly test negative?
#   P(-|ND)

# Has the disease, "D"
# Disease free, "ND"
# Tested positive for the disease, "+"
# Tested negative, "-"

# QUESTION: You tested positive, but have no symptoms. What is the probability
# that you have the disease?
#     P(D|+), probability of disease given a positive test


# Equation to solve: P(D|+) = [P(D) * P(+|D)] / P(+)
# P(+) = ???
# P(D) = 0.005
# P(+|D) = 0.99
# P(D|+) = ???

# 1) Solve P(+)
# 5 people have the disease, how many test positive?
# 995 are disease-free, how many test positive?
sensitivity <- 0.99 # how many disease correctly test positive?
specificity <- 0.95 # how many disease-free correctly test negative?
diseased <- 5
disease_free <- 995
total_pop <- 1000
true_pos_count <- diseased * sensitivity # how many correct positives
false_pos_count <- disease_free * (1 - specificity) # how many false positives
total_pos_count <- true_pos_count + false_pos_count
prob_pos <- total_pos_count / total_pop # P(+) = 0.0547

# 2) Solve for P(D|+)
prob_dis <- 0.005
prob_pos_dis <-0.99
prob_dis_pos <-(prob_dis * prob_pos_dis) / prob_pos
paste("The probability of being diseased given a positive test it", prob_dis_pos * 100, "%")









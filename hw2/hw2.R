# Load the data
dt <- read.csv("./Equity_Premium.csv")
time <- dt$Time
y <- dt$y
x1 <- dt$x_dfy  # We'll use this as our condition variable
x2 <- dt$x_tms
x3 <- dt$x_dp

# Calculate conditional means
mean_y_high <- mean(y[x1 > 0.015])
mean_y_low <- mean(y[x1 <= 0.015])

# Calculate proportions for weighting
prop_high <- mean(x1 > 0.015)
prop_low <- mean(x1 <= 0.015)

# Set a1 and a2 (you might want to adjust these based on your specific requirements)
a1 <- prop_high
a2 <- prop_low

# Calculate E[Y] according to the given equation
E_Y <- a1 * mean_y_high + a2 * mean_y_low

# Print results
cat("E[Y|X1 > 0.15] =", mean_y_high, "\n")
cat("E[Y|X1 <= 0.15] =", mean_y_low, "\n")
cat("Proportion where X1 > 0.15:", prop_high, "\n")
cat("Proportion where X1 <= 0.15:", prop_low, "\n")
cat("E[Y] =", E_Y, "\n")


cond1 <- x1 > 0.015
cond2 <- x2 > 0.02

# Calculate conditional means for each case
mean_y_case1 <- mean(y[cond1 & cond2])    # x1 > 0.015 and x2 > 0.02
mean_y_case2 <- mean(y[cond1 & !cond2])   # x1 > 0.015 and x2 <= 0.02
mean_y_case3 <- mean(y[!cond1 & cond2])   # x1 <= 0.015 and x2 > 0.02
mean_y_case4 <- mean(y[!cond1 & !cond2])  # x1 <= 0.015 and x2 <= 0.02

# Calculate proportions for each case (a1, a2, a3, a4)
a1 <- mean(cond1 & cond2)
a2 <- mean(cond1 & !cond2)
a3 <- mean(!cond1 & cond2)
a4 <- mean(!cond1 & !cond2)

# Calculate E[Y] according to the four-case equation
E_Y <- a1 * mean_y_case1 + a2 * mean_y_case2 + a3 * mean_y_case3 + a4 * mean_y_case4

# Print results
cat("Conditional Means:\n")
cat("E[Y|X1 > 0.015 and X2 > 0.02] =", mean_y_case1, "\n")
cat("E[Y|X1 > 0.015 and X2 <= 0.02] =", mean_y_case2, "\n")
cat("E[Y|X1 <= 0.015 and X2 > 0.02] =", mean_y_case3, "\n")
cat("E[Y|X1 <= 0.015 and X2 <= 0.02] =", mean_y_case4, "\n\n")

cat("Proportions:\n")
cat("a1 (X1 > 0.015 and X2 > 0.02):", a1, "\n")
cat("a2 (X1 > 0.015 and X2 <= 0.02):", a2, "\n")
cat("a3 (X1 <= 0.015 and X2 > 0.02):", a3, "\n")
cat("a4 (X1 <= 0.015 and X2 <= 0.02):", a4, "\n\n")

cat("E[Y] =", E_Y, "\n")

# Verification
cat("Sum of proportions:", a1 + a2 + a3 + a4, "\n")  # This should equal 1


cond_x1 <- x1 > 0.015
cond_x3 <- x3 > -4

# Calculate conditional means for each case, given X3 > -4
mean_y_case1 <- mean(y[cond_x1 & cond_x3])     # X1 > 0.015 and X3 > -4
mean_y_case2 <- mean(y[!cond_x1 & cond_x3])    # X1 <= 0.015 and X3 > -4

# Calculate proportions for each case (a1, a2), given X3 > -4
total_x3_condition <- sum(cond_x3)
a1 <- sum(cond_x1 & cond_x3) / total_x3_condition
a2 <- sum(!cond_x1 & cond_x3) / total_x3_condition

# Calculate E[Y|X3 > -4]
E_Y_given_X3 <- a1 * mean_y_case1 + a2 * mean_y_case2

# Print results
cat("Conditional Means:\n")
cat("E[Y|X1 > 0.015 and X3 > -4] =", mean_y_case1, "\n")
cat("E[Y|X1 <= 0.015 and X3 > -4] =", mean_y_case2, "\n\n")

cat("Proportions (given X3 > -4):\n")
cat("a1 (X1 > 0.015 | X3 > -4):", a1, "\n")
cat("a2 (X1 <= 0.015 | X3 > -4):", a2, "\n\n")

cat("E[Y|X3 > -4] =", E_Y_given_X3, "\n")

# Verification
cat("Sum of proportions:", a1 + a2, "\n")  # This should equal 1

# Additional information
cat("\nAdditional Information:\n")
cat("Proportion of data where X3 > -4:", mean(cond_x3), "\n")
cat("Total number of observations:", length(y), "\n")
cat("Number of observations where X3 > -4:", sum(cond_x3), "\n")
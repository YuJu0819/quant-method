# # Set seed for reproducibility
# set.seed(69)

# # Number of simulations
# n <- 1000000

# # Generate X from t-distribution with 3 degrees of freedom
# X <- rt(n, df = 3)

# # Calculate Y
# Y <- 1 / (1 + X^4)

# # Fit linear regression
# model <- lm(Y ~ X)

# # Extract the slope (b)
# b <- coef(model)[2]

# # Print the result
# cat("Estimated b:", b, "\n")

# # Calculate R-squared to check the quality of the linear approximation
# r_squared <- summary(model)$r.squared
# cat("R-squared:", r_squared, "\n")

# # Plot the results
# plot(X, Y, main = "Scatter plot of Y vs X", xlab = "X", ylab = "Y", pch = ".", col = "blue")
# abline(model, col = "red")

# # Add a legend
# legend("topright", legend = c("Data", "Linear Fit"), 
#        col = c("blue", "red"), pch = c(".", NA), lty = c(NA, 1))

# # Calculate and print the mean of Y
# mean_Y <- mean(Y)
# cat("Estimated E[Y]:", mean_Y, "\n")

# # Calculate and print the correlation between X and Y
# correlation <- cor(X, Y)
# cat("Correlation between X and Y:", correlation, "\n")
# Set seed for reproducibility
set.seed(69)

# Number of simulations
n <- 1000000

# Generate X from t-distribution with 3 degrees of freedom
X <- rt(n, df = 3)

# Calculate Y
Y <- 1 / (1 + X^4)

# Calculate E[XY]
E_XY <- mean(X * Y)

# Calculate E[X^2]
E_X2 <- mean(X^2)

# Calculate the slope using E[XY]/E[X^2]
b_formula <- E_XY / E_X2

# Print the result
cat("Estimated b using E[XY]/E[X^2]:", b_formula, "\n")

# Fit linear regression for comparison
model <- lm(Y ~ X)
b_lm <- coef(model)[2]
cat("Estimated b using lm():", b_lm, "\n")

# Calculate R-squared
r_squared <- summary(model)$r.squared
cat("R-squared:", r_squared, "\n")

# Plot the results
plot(X, Y, main = "Scatter plot of Y vs X", xlab = "X", ylab = "Y", pch = ".", col = "blue")
abline(a = mean(Y) - b_formula * mean(X), b = b_formula, col = "red")

# Add a legend
legend("topright", legend = c("Data", "Linear Fit"), 
       col = c("blue", "red"), pch = c(".", NA), lty = c(NA, 1))

# Calculate and print the mean of Y
mean_Y <- mean(Y)
cat("Estimated E[Y]:", mean_Y, "\n")

# Calculate and print the correlation between X and Y
correlation <- cor(X, Y)
cat("Correlation between X and Y:", correlation, "\n")
# Load necessary libraries
library(car)  # For the linearHypothesis function

# Read the data
# Replace 'Equity_Premium.csv' with the correct path if necessary
dt<- read.csv("Equity_Premium.csv")

# Check the structure of the data
str(data)
head(data)

# The data should contain the following columns:
# Y_i: Equity premium (dependent variable)
# x_dfy, x_infl, x_svar, x_tms, x_tbl, x_dfr, x_ltr, x_ep, x_bmr, x_ntis (independent variables)
dt <- read.csv("./Equity_Premium.csv")
y <- dt$y
ones <- rep(1, 504)
dfy <- dt$x_dfy
infl <- dt$x_infl
svar <- dt$x_svar
tms <- dt$x_tms
tbl <- dt$x_tbl
dfr <- dt$x_dfr
dp <- dt$x_dp
ltr <- dt$x_ltr
ep <- dt$x_ep
bmr <- dt$x_bmr
ntis <- dt$x_ntis

x <- cbind(ones, dfy, infl, svar, tms,
           tbl, dfr, dp, ltr, ep, bmr, ntis)


# Part 1
#
model <- lm(y ~ (x- 1 ))

# Part a: Individual Wald tests for each coefficient

# View the summary of the regression model
summary(model)

# Part (a): Testing H0: beta_j = 0 for each coefficient

# Extract the coefficients and their standard errors
coefficients_summary <- summary(model)$coefficients

# coefficients_summary is a matrix with the following columns:
# Estimate, Std. Error, t value, Pr(>|t|)

# Display the coefficients summary
print(coefficients_summary)

# Significance level
alpha <- 0.05

# Create a data frame to summarize the hypothesis testing results
results <- data.frame(
  Coefficient = rownames(coefficients_summary),
  Estimate = coefficients_summary[, "Estimate"],
  Std_Error = coefficients_summary[, "Std. Error"],
  t_value = coefficients_summary[, "t value"],
  p_value = coefficients_summary[, "Pr(>|t|)"],
  Reject_H0 = coefficients_summary[, "Pr(>|t|)"] < alpha
)

# Display the results
print(results)

# Interpret the results for each coefficient
cat("\nPart (a): Hypothesis Testing Results for Each Coefficient\n")
for (i in 1:nrow(results)) {
  cat("Coefficient:", results$Coefficient[i], "\n")
  cat("Estimate:", results$Estimate[i], "\n")
  cat("Standard Error:", results$Std_Error[i], "\n")
  cat("t-value:", results$t_value[i], "\n")
  cat("p-value:", results$p_value[i], "\n")
  if (results$Reject_H0[i]) {
    cat("Conclusion: Reject H0 at the 5% significance level.\n\n")
  } else {
    cat("Conclusion: Fail to reject H0 at the 5% significance level.\n\n")
  }
}

# Part (b): Testing H0: beta_1 = 0 and beta_2 + beta_3 = 0

# Note:
# beta_1 corresponds to the Intercept (since we included it in the model)
# beta_2 corresponds to x_dfy
# beta_3 corresponds to x_infl

# Use the linearHypothesis function to perform the Wald test for joint hypotheses
# Specify the null hypotheses as character strings

# Hypotheses:
# H0: (Intercept) = 0
# H0: x_dfy + x_infl = 0

# Perform the Wald test
wald_test <- linearHypothesis(model,
                              c("xdfy + xinfl = 0"),
                              test = "Chisq")

# Display the test results
print(wald_test)

# Extract the test statistic, degrees of freedom, and p-value
chi_sq_stat <- wald_test$Chisq[2]  # Test statistic for the hypotheses
df <- wald_test$Df[2]              # Degrees of freedom
p_value <- wald_test$`Pr(>Chisq)`[2]  # p-value for the test

# Determine whether to reject H0
reject_H0 <- p_value < alpha

# Output the test results and conclusion
cat("\nPart (b): Wald Test for Joint Hypotheses\n")
cat("Null Hypotheses:\n")
cat("H0: (Intercept) = 0\n")
cat("H0: x_dfy + x_infl = 0\n\n")
cat("Chi-squared statistic:", chi_sq_stat, "\n")
cat("Degrees of freedom:", df, "\n")
cat("p-value:", p_value, "\n")

if (reject_H0) {
  cat("Conclusion: Reject H0 at the 5% significance level.\n")
} else {
  cat("Conclusion: Fail to reject H0 at the 5% significance level.\n")
}

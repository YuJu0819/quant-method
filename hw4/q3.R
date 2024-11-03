dt <- read.csv("./Equity_Premium.csv")
time <- dt$Time
y <- dt$y
x_dfy <- dt$x_dfy  # We'll use this as our condition variable
x_infl <- dt$x_infl
x_svar <- dt$x_svar
x_tms <- dt$x_tms
x_tbl <- dt$x_tbl
x_dfr <- dt$x_dfr
x_dp <-dt$x_dp
x_ltr <- dt$x_ltr
# Step 1: Prepare the design matrix and fit the model
X <- cbind(1, x_dfy, x_infl, x_svar, x_tms, x_tbl, x_dfr, x_dp , x_ltr)
model <- lm(y ~ X - 1)  # -1 to exclude intercept since we include it in X

# Step 2: Compute residuals
residuals <- residuals(model)
mean(residuals)
n <- length(residuals)  # number of observations
# n

# Step 3: Estimate sigma^2 (variance of residuals)
sigma_hat_squared <- mean(residuals^2)

# Step 4: Standardize the residuals
# std_residuals <- sqrt(sigma_hat_squared)
std_residuals <- residuals / sqrt(sigma_hat_squared)

# Step 5: Compute skewness (sk) and kurtosis (kr)

skewness <- mean(std_residuals^3)
skewness
kurtosis <- mean(std_residuals^4)

# Step 6: Compute Jarque-Bera test statistic
JB_stat <- n * ((skewness^2 / 6) + ((kurtosis - 3)^2 / 24))

# Step 7: Critical values for chi-squared distribution
alpha_1 <- 0.01
alpha_5 <- 0.05
alpha_10 <- 0.10

# JB is compared to the quantiles of chi-squared distribution with 2 degrees of freedom
critical_value_1 <- qchisq(1 - alpha_1, df = 2)
critical_value_5 <- qchisq(1 - alpha_5, df = 2)
critical_value_10 <- qchisq(1 - alpha_10, df = 2)

# Step 8: Test results
JB_test_results <- data.frame(
  JB_stat = JB_stat,
  Critical_1_percent = critical_value_1,
  Critical_5_percent = critical_value_5,
  Critical_10_percent = critical_value_10,
  Reject_1_percent = JB_stat > critical_value_1,
  Reject_5_percent = JB_stat > critical_value_5,
  Reject_10_percent = JB_stat > critical_value_10
)

# Step 9: Display the JB test results
JB_test_results

# Step 10: Kernel density estimation and comparison with N(0,1)
# Plot the density estimate of standardized residuals vs N(0,1)
library(ggplot2)

# Generate the kernel density estimate
density_estimate <- density(std_residuals)

# Generate a sequence of values for the N(0,1) normal distribution
x_vals <- seq(min(density_estimate$x), max(density_estimate$x), length.out = 100)
normal_density <- dnorm(x_vals)

# Plot the results
df_density <- data.frame(x = density_estimate$x, y = density_estimate$y)
df_normal <- data.frame(x = x_vals, y = normal_density)

ggplot() +
  geom_line(data = df_density, aes(x = x, y = y), color = "blue", size = 1.5, linetype = "dashed") +
  geom_line(data = df_normal, aes(x = x, y = y), color = "red", size = 1.5) +
  ggtitle("Kernel Density Estimate vs N(0,1) Density") +
  labs(x = "Standardized Residuals", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

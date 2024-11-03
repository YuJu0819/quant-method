
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


# Step 3: Extract LS estimator (coefficients)
beta_hat <- coef(model)

# Step 4: Compute variance-covariance matrix (for s_V)
X_t_X_inv <- solve(t(X) %*% X)
s_V <- summary(model)$sigma
std_errors_V <- s_V * sqrt(diag(X_t_X_inv))  # Standard deviation estimates s_V

# Step 5: Compute robust standard errors (for s_W)
# library(sandwich)
# vcov_robust <- vcovHC(model, type = "HC")  # Using heteroscedasticity-consistent covariance matrix
# std_errors_W <- sqrt(diag(vcov_robust))  # Standard deviation estimates s_W

# # Step 6: Print results
# beta_hat
# std_errors_V
# std_errors_W

# Step 5: Compute the t-statistics
t_stat <- beta_hat / std_errors_V

# Step 6: Critical t-values for the two-tailed test
alpha_1 <- 0.01
alpha_5 <- 0.05
alpha_10 <- 0.10
n <- nrow(X)  # Number of observations
p <- length(beta_hat)  # Number of predictors
df <- n - p  # Degrees of freedom

# Critical values for the two-tailed test (use qt for t-distribution)
critical_t_1 <- qt(1 - alpha_1 / 2, df)
critical_t_5 <- qt(1 - alpha_5 / 2, df)
critical_t_10 <- qt(1 - alpha_10 / 2, df)

# Step 7: Hypothesis testing results
test_results <- data.frame(
  Beta = beta_hat,
  Std_Error = std_errors_V,
  T_Statistic = t_stat,
  Significant_1_percent = abs(t_stat) > critical_t_1,
  Significant_5_percent = abs(t_stat) > critical_t_5,
  Significant_10_percent = abs(t_stat) > critical_t_10
)

# Step 8: Display the results
test_results
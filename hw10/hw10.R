# Load required packages
library(stats)

# Load the data
data <- read.csv('Equity_Premium.csv')
bull_periods <- list(
  c(20:80),     # August 1982 to August 1987
  c(84:231),    # December 1987 to March 2000
  c(249:253),   # September 2001 to January 2002
  c(262:322),   # October 2002 to October 2007
  c(339:470),   # March 2009 to February 2020
  c(471:493),   # March 2020 to January 2022
  c(502:504)    # October 2022 to December 2022
)
# Extract the response variable Y_i and predictor variables X_i
# Assuming 'Y' is the market-cycle index in the data
Y <- rep(1, nrow(data))
for(period in bull_periods) {
  Y[period] <- 0
}
# Predictor variables as specified
X_vars <- c('x_dfy', 'x_infl', 'x_svar', 'x_tms', 'x_tbl', 'x_dfr',
            'x_dp', 'x_ltr', 'x_ep', 'x_bmr', 'x_ntis')
X <- data[, X_vars]

# Add intercept term
X <- as.matrix(cbind(Intercept = 1, X))

# Estimate the Linear Probability Model (LPM) using least squares
lpm_model <- lm(Y ~ X - 1)  # '-1' to exclude the default intercept
beta_ls <- coef(lpm_model)

# Set initial values for optimization
init_values <- beta_ls

# Negative log-likelihood function for the probit model
neg_loglik_probit <- function(beta, X, Y) {
  eta <- X %*% beta
  p <- pnorm(eta)

  loglik <- Y * log(p) + (1 - Y) * log(1 - p)
  return(-sum(loglik))
}

# Negative log-likelihood function for the logit model
neg_loglik_logit <- function(beta, X, Y) {
  eta <- X %*% beta
  p <- plogis(eta)

  loglik <- Y * log(p) + (1 - Y) * log(1 - p)
  return(-sum(loglik))
}

# Optimize the probit model
result_probit <- optim(par = init_values, fn = neg_loglik_probit, method = "BFGS",
                    control = list(maxit = 1000), X = X, Y = Y)

# Check convergence
if (result_probit$convergence == 0) {
  cat("Probit model optimization converged.\n")
} else {
  cat("Probit model optimization did not converge.\n")
}

beta_mle_probit <- result_probit$par

# Optimize the logit model
result_logit <- optim(par = init_values, fn = neg_loglik_logit, method = "BFGS",
                    control = list(maxit = 1000), X = X, Y = Y)

# Check convergence
if (result_logit$convergence == 0) {
  cat("Logit model optimization converged.\n")
} else {
  cat("Logit model optimization did not converge.\n")
}

beta_mle_logit <- result_logit$par

# Calculate the predicted probabilities
prob_lpm <- X %*% beta_ls
prob_probit <- pnorm(X %*% beta_mle_probit)
prob_logit <- plogis(X %*% beta_mle_logit)

# Plotting the results
time_index <- 1:length(Y)
plot(time_index, Y, type = 'l', col = 'black', lwd = 2, ylim = c(0, 1),
     ylab = 'Probability / Y_i', xlab = 'Time Index',
     main = 'Market Cycle Index and Estimated Probabilities')
lines(time_index, prob_lpm, col = 'red', lwd = 2)
lines(time_index, prob_probit, col = 'blue', lwd = 2)
lines(time_index, prob_logit, col = 'green', lwd = 2)
legend('topright', legend = c('Y_i', 'LPM', 'Probit', 'Logit'),
       col = c('black', 'red', 'blue', 'green'), lwd = 2)

# Evaluate the score functions at MLEs
# Probit model score function
gradient_probit <- function(beta, X, Y) {
  eta <- X %*% beta
  p <- pnorm(eta)
  phi <- dnorm(eta)
  grad <- t(X) %*% ((Y - p) * (phi / (p * (1 - p) + 1e-15)))
  return(grad)
}

score_probit <- gradient_probit(beta_mle_probit, X, Y)
cat("Score function at MLE for the probit model:\n")
print(score_probit)

# Logit model score function
gradient_logit <- function(beta, X, Y) {
  eta <- X %*% beta
  p <- plogis(eta)
  grad <- t(X) %*% (Y - p)
  return(grad)
}

score_logit <- gradient_logit(beta_mle_logit, X, Y)
cat("Score function at MLE for the logit model:\n")
print(score_logit)

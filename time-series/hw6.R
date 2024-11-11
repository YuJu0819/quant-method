library(astsa)
# # data(cmort)
# (reg1 = ar.ols(cmort, order=2)) # coefs: [1] 0.4286 [2] 0.4418; sigma^2 estimated as 32.32
# (reg2 = ar.yw(cmort, order=2)) # coefs: [1] 0.4339 [2] 0.4376 ; sigma^2 estimated as 32.84
# (reg1$asy.se.coef) # se: [1] 0.0397 [2] 0.0397 (sqrt(diag(reg2$asy.var.coef))) # s
# (sqrt(diag(reg2$asy.var.coef)))
# x = arima.sim(list(order=c(1,0,1), ar=.01, ma=-.01), n=500)
#  plot(x)
#  acf2(x)
#  sarima(x, 1, 0, 1)

# # Set parameters
# n <- 500               # Number of data points
# true_a <- 0.9          # True value of a
# true_delta <- 5        # True time delay
# sigma_s <- 1           # Standard deviation of s_t (square root of variance)

# # Generate white Gaussian noise for s_t
# set.seed(123)          # Seed for reproducibility
# s <- rnorm(n + true_delta, mean = 0, sd = sigma_s)  # Extra points to handle lag

# # Generate x_t based on true parameters
# x <- numeric(n)
# for (t in (true_delta + 1):(n + true_delta)) {
#   x[t - true_delta] <- s[t] + true_a * s[t - true_delta]
# }

# # Define function to calculate log-likelihood for given a, delta, and sigma_s
# log_likelihood <- function(a, delta, x, sigma_s) {
#   n <- length(x)
#   residuals <- numeric(n - delta)
#   for (t in (delta + 1):n) {
#     residuals[t - delta] <- x[t] - a * x[t - delta]
#   }
#   # Calculate log-likelihood assuming Gaussian residuals
#   log_lik <- -0.5 * sum((residuals / sigma_s)^2) - (n - delta) * log(sigma_s) - 0.5 * (n - delta) * log(2 * pi)
#   return(log_lik)
# }

# # Search for the best delta by maximizing the log-likelihood
# candidate_deltas <- 3:7
# best_log_lik <- -Inf
# best_delta <- NA

# # Loop over candidate deltas and estimate log-likelihood
# for (delta in candidate_deltas) {
#   log_lik <- log_likelihood(true_a, delta, x, sigma_s)
#   if (log_lik > best_log_lik) {
#     best_log_lik <- log_lik
#     best_delta <- delta
#   }
# }

# # Display results
# cat("Estimated delta:", best_delta, "\n")
# cat("Log-likelihood for estimated delta:", best_log_lik, "\n")


plot(gtemp_both)
plot(diff(gtemp_both))
acf2(gtemp_both)
 sarima(gtemp_both, 1, 1, 2)
 sarima.for(gtemp_both, 20, 1,1, 1)
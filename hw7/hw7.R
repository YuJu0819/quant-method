# Set the seed for reproducibility
set.seed(12345)

# Define constants
n_list <- c(50, 100, 200, 500)
k <- 5
Nrep <- 1000  # Number of replications
alpha <- 0.05  # Nominal size

# Define R and theta0
R <- matrix(c(1,0,0,0,0,0,
              1,1,0,0,0,0), nrow=2, byrow=TRUE)
theta0 <- c(1, 2)

# Degrees of freedom for the chi-squared test
df <- nrow(R)

# Part (a): Empirical size under H0
beta_H0 <- rep(1, k+1)

size_results <- data.frame(n = n_list, Empirical_Size = NA)

for (n_idx in 1:length(n_list)) {
  n <- n_list[n_idx]
  rejections <- 0
  
  for (rep in 1:Nrep) {
    # Generate data
    X <- matrix(rnorm(n * (k+1)), nrow = n, ncol = k+1)
    epsilon <- rnorm(n)
    Y <- X %*% beta_H0 + epsilon
    
    # Estimate beta_hat
    beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
    
    # Estimate variance of residuals
    residuals <- Y - X %*% beta_hat
    sigma2_hat <- sum(residuals^2) / (n - (k+1))
    
    # Estimate covariance matrix of beta_hat
    cov_beta_hat <- sigma2_hat * solve(t(X) %*% X)
    
    # Compute the test statistic W
    W <- t(R %*% beta_hat - theta0) %*% solve(R %*% cov_beta_hat %*% t(R)) %*% (R %*% beta_hat - theta0)
    W <- as.numeric(W)
    
    # Compute p-value
    p_value <- 1 - pchisq(W, df)
    
    # Check if we reject H0
    if (p_value < alpha) {
      rejections <- rejections + 1
    }
  }
  
  # Calculate empirical size
  empirical_size <- rejections / Nrep
  size_results$Empirical_Size[n_idx] <- empirical_size
}

print("Part (a): Empirical Size under H0")
print(size_results)

# Part (b): Empirical power under alternative
beta_H1 <- 1:(k+1)  # beta = (1,2,3,4,5,6)'

power_results <- data.frame(n = n_list, Empirical_Power = NA)

for (n_idx in 1:length(n_list)) {
  n <- n_list[n_idx]
  rejections <- 0
  
  for (rep in 1:Nrep) {
    # Generate data
    X <- matrix(rnorm(n * (k+1)), nrow = n, ncol = k+1)
    epsilon <- rnorm(n)
    Y <- X %*% beta_H1 + epsilon
    
    # Estimate beta_hat
    beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
    
    # Estimate variance of residuals
    residuals <- Y - X %*% beta_hat
    sigma2_hat <- sum(residuals^2) / (n - (k+1))
    
    # Estimate covariance matrix of beta_hat
    cov_beta_hat <- sigma2_hat * solve(t(X) %*% X)
    
    # Compute the test statistic W
    W <- t(R %*% beta_hat - theta0) %*% solve(R %*% cov_beta_hat %*% t(R)) %*% (R %*% beta_hat - theta0)
    W <- as.numeric(W)
    
    # Compute p-value
    p_value <- 1 - pchisq(W, df)
    
    # Check if we reject H0
    if (p_value < alpha) {
      rejections <- rejections + 1
    }
  }
  
  # Calculate empirical power
  empirical_power <- rejections / Nrep
  power_results$Empirical_Power[n_idx] <- empirical_power
}

print("Part (b): Empirical Power under alternative")
print(power_results)

# Part (c): Empirical power under local alternatives
h_values <- 1:10
local_power_results <- data.frame()

for (n_idx in 1:length(n_list)) {
  n <- n_list[n_idx]
  n_sqrt_inv <- 1 / sqrt(n)
  
  for (h in h_values) {
    rejections <- 0
    beta_local <- rep(1 + n_sqrt_inv * h, k+1)
    
    for (rep in 1:Nrep) {
      # Generate data
      X <- matrix(rnorm(n * (k+1)), nrow = n, ncol = k+1)
      epsilon <- rnorm(n)
      Y <- X %*% beta_local + epsilon
      
      # Estimate beta_hat
      beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
      
      # Estimate variance of residuals
      residuals <- Y - X %*% beta_hat
      sigma2_hat <- sum(residuals^2) / (n - (k+1))
      
      # Estimate covariance matrix of beta_hat
      cov_beta_hat <- sigma2_hat * solve(t(X) %*% X)
      
      # Compute the test statistic W
      W <- t(R %*% beta_hat - theta0) %*% solve(R %*% cov_beta_hat %*% t(R)) %*% (R %*% beta_hat - theta0)
      W <- as.numeric(W)
      
      # Compute p-value
      p_value <- 1 - pchisq(W, df)
      
      # Check if we reject H0
      if (p_value < alpha) {
        rejections <- rejections + 1
      }
    }
    
    # Calculate empirical power
    empirical_power <- rejections / Nrep
    
    # Store results
    local_power_results <- rbind(local_power_results, data.frame(n = n, h = h, Empirical_Power = empirical_power))
  }
}

print("Part (c): Empirical Power under local alternatives")
print(local_power_results)

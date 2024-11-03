# Set seed for reproducibility
set.seed(12345)

# Define parameters
n_values <- c(10, 50, 100)  # Sample sizes
distributions <- c("normal", "t2")  # Distributions: normal and t with 2 degrees of freedom
B <- 1000  # Number of replications

# Create a function to perform simulations
simulate_statistics <- function(n, dist, B) {
  # Initialize vectors to store statistics
  mean_Y <- numeric(B)
  sqrt_n_mean_Y <- numeric(B)
  
  for (b in 1:B) {
    # Generate sample based on the specified distribution
    if (dist == "normal") {
      Y_sample <- rnorm(n, mean = 0, sd = 1)
    } else if (dist == "t2") {
      Y_sample <- rt(n, df = 2)
    }
    
    # Compute statistics
    mean_Y[b] <- mean(Y_sample)
    sqrt_n_mean_Y[b] <- sqrt(n) * mean_Y[b]
  }
  
  # Return a list of statistics
  return(list(mean_Y = mean_Y, sqrt_n_mean_Y = sqrt_n_mean_Y))
}

# Load necessary library for plotting
library(ggplot2)

# Loop over combinations of n and distributions
for (n in n_values) {
  for (dist in distributions) {
    # Perform simulations
    stats <- simulate_statistics(n, dist, B)
    
    # Prepare data for plotting
    data_mean_Y <- data.frame(value = stats$mean_Y, Statistic = "mean_Y")
    data_sqrt_n_mean_Y <- data.frame(value = stats$sqrt_n_mean_Y, Statistic = "sqrt_n_mean_Y")
    data_all <- rbind(data_mean_Y, data_sqrt_n_mean_Y)
    
    # Plot KDEs and compare with N(0,1)
    p <- ggplot(data_all, aes(x = value, color = Statistic)) +
      geom_density(linewidth = 1) +
      stat_function(fun = dnorm, args = list(mean = 0, sd = 1), aes(color = "N(0,1)"), linewidth = 1, linetype = "dashed") +
      labs(title = paste("n =", n, ", Distribution:", ifelse(dist == "normal", "N(0,1)", "t(2)")),
           x = "Value", y = "Density") +
      scale_color_manual(name = "Statistic",
                         values = c("mean_Y" = "blue", "sqrt_n_mean_Y" = "green", "N(0,1)" = "red"),
                         labels = c("mean_Y" = expression(bar(Y)),
                                    "sqrt_n_mean_Y" = expression(sqrt(n) * bar(Y)),
                                    "N(0,1)" = "N(0,1)")) +
      theme_minimal()
    
    # Print the plot
    print(p)
  }
}

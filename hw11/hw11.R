# Load necessary libraries
# install.packages("gridExtra", repos = "https://cran.csie.ntu.edu.tw/")
library(readr)      # For reading CSV files
library(ggplot2)    # For plotting
library(gridExtra)  # For arranging multiple plots
library(forecast)   # For ACF and Box-Pierce test functions
library(tseries)    # Alternative for Box-Pierce test

# Set the working directory to the location of 'Equity-Premium.csv' if needed
# setwd("path_to_your_directory")

# Load the data
data <- read_csv("Equity_Premium.csv")

# List of variables to analyze
variables <- c("y", "x_dfy", "x_infl", "x_svar", "x_tms", "x_tbl",
               "x_dfr", "x_dp", "x_ltr", "x_ep", "x_bmr", "x_ntis")

# Initialize a list to store plots
plot_list <- list()

# Loop over each variable
for (var in variables) {
  # Extract the time series data and remove missing values
  ts_data <- na.omit(data[[var]])
  
  # Calculate the length of the time series
  T <- length(ts_data)
  
  # Set maximum lag
  max_lag <- 24
  
  # Calculate ACF values
  acf_result <- acf(ts_data, plot = FALSE, lag.max = max_lag)
  
  # Extract ACF values and lags
  acf_values <- acf_result$acf[-1]  # Exclude lag 0
  lags <- acf_result$lag[-1]
  
  # Calculate 95% confidence interval
  ci <- qnorm(0.975) / sqrt(T)
  
  # Create a data frame for plotting
  acf_df <- data.frame(
    Lag = lags,
    ACF = acf_values
  )
  
  # Plot the ACF with confidence intervals
  p <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = "red") +
    labs(title = paste("ACF for", var),
         x = "Lag",
         y = "Autocorrelation") +
    theme_minimal()
  # Add the plot to the list
  plot_list[[var]] <- p
  
  # Perform the Box-Pierce test for m = 12
  bp_test_12 <- Box.test(ts_data, lag = 12, type = "Box-Pierce")
  
  # Perform the Box-Pierce test for m = 24
  bp_test_24 <- Box.test(ts_data, lag = 24, type = "Box-Pierce")
  
  # Output the test results
  cat("\n---------------------------------------------------\n")
  cat("Variable:", var, "\n")
  cat("Length of Time Series (T):", T, "\n")
  
  # Box-Pierce test for m = 12
  cat("\nBox-Pierce Test Results for m = 12:\n")
  cat("Statistic (Q):", round(bp_test_12$statistic, 2), "\n")
  cat("Degrees of Freedom:", bp_test_12$parameter, "\n")
  cat("P-value:", round(bp_test_12$p.value, 4), "\n")
  
  if (bp_test_12$p.value < 0.05) {
    cat("Conclusion: Reject the null hypothesis of IIDness at the 5% level.\n")
  } else {
    cat("Conclusion: Fail to reject the null hypothesis of IIDness at the 5% level.\n")
  }
  
  # Box-Pierce test for m = 24
  cat("\nBox-Pierce Test Results for m = 24:\n")
  cat("Statistic (Q):", round(bp_test_24$statistic, 2), "\n")
  cat("Degrees of Freedom:", bp_test_24$parameter, "\n")
  cat("P-value:", round(bp_test_24$p.value, 4), "\n")
  
  if (bp_test_24$p.value < 0.05) {
    cat("Conclusion: Reject the null hypothesis of IIDness at the 5% level.\n")
  } else {
    cat("Conclusion: Fail to reject the null hypothesis of IIDness at the 5% level.\n")
  }
}

# Arrange and display the plots (optional)
# You can adjust the number of plots per page as needed
num_plots <- length(plot_list)
plots_per_page <- 4
num_pages <- ceiling(num_plots / plots_per_page)

for (i in 1:num_pages) {
  start_idx <- (i - 1) * plots_per_page + 1
  end_idx <- min(i * plots_per_page, num_plots)
  grid_plots <- plot_list[start_idx:end_idx]
  do.call("grid.arrange", c(grid_plots, ncol = 2))
}

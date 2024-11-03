library(tidyverse)

# Read the CSV data (assuming it's in the same format as before)
dt <- read.csv("./Equity_Premium.csv")

# Function to calculate R-squared for a given formula
calculate_r_squared <- function(formula, data) {
  model <- lm(formula, data = data)
  return(summary(model)$r.squared)
}
dt <- read.csv("./Equity_Premium.csv")
time <- dt$Time
y <- dt$y
x_dfy <- dt$x_dfy  # We'll use this as our condition variable
x_infl <- dt$x_infl
x_svar <- dt$x_svar
x_tms <- dt$x_tms
x_tbl <- dt$x_tbl
x_dfr <- dt$x_dfr
# Define the formulas for each regression
formulas <- list(
    y ~ 1,
  y ~ 1 + x_dfy,
  y ~ 1 + x_dfy + x_infl,
  y ~ 1 + x_dfy + x_infl + x_svar,
  y ~ 1 + x_dfy + x_infl + x_svar + x_tms,
  y ~ 1 + x_dfy + x_infl + x_svar + x_tms + x_tbl,
  y ~ 1 + x_dfy + x_infl + x_svar + x_tms + x_tbl + x_dfr
)
# Calculate R-squared for each formula
r_squared_values <- sapply(formulas, calculate_r_squared, data = dt)
# Create a data frame with the results
results <- data.frame(
  Model = c("M1", "M2", "M3", "M4", "M5", "M6", "M7"),
  R_squared = r_squared_values
)
# Print the results
print(results)
# Calculate the average R-squared
average_r_squared <- mean(r_squared_values)
cat("\nAverage R-squared:", average_r_squared)
# Plot the R-squared values
ggplot(results, aes(x = Model, y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "R-squared Values for Different Models",
       x = "Model",
       y = "R-squared")

# Save the plot (uncomment if you want to save it)
# ggsave("r_squared_plot.png", width = 8, height = 6)
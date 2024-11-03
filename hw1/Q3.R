dt <- read.csv("./Equity_Premium.csv")
time <- dt$Time
y <- dt$y
x_dfy <- dt$x_dfy
x_infl <- dt$x_infl
x_svar <- dt$x_svar
x_tms <- dt$x_tms
x_tbl <- dt$x_tbl
x_dfr <- dt$x_dfr
x_dp <- dt$x_dp
x_ltr <- dt$x_ltr
x_ep <- dt$x_ep
x_bmr <- dt$x_bmr
x_ntis <- dt$x_ntis

variables <- list(y, x_dfy, x_infl, x_svar, x_tms, x_tbl, x_dfr, x_dp, x_ltr, x_ep, x_bmr, x_ntis)
var_names <- c("y", "x_dfy", "x_infl", "x_svar", "x_tms", "x_tbl", "x_dfr", "x_dp", "x_ltr", "x_ep", "x_bmr", "x_ntis")
y <- variables[[1]]
variables <- variables[-1]
variables <- do.call(cbind, variables)

transposed_variables <- t(variables)
tmp_matrix <- transposed_variables %*% variables

eigen_result <- eigen(tmp_matrix)
eigenvalues <- eigen_result$values

# Sort eigenvalues in descending order
sorted_eigenvalues <- sort(eigenvalues, decreasing = TRUE)

# Create a data frame for plotting
plot_data <- data.frame(
  Component = 1:11,
  Eigenvalue = sorted_eigenvalues[1:11]
)

# Create the scree plot
png("./plots/scree_plot.png", width = 800, height = 600)

# Plot eigenvalues
plot(plot_data$Component, plot_data$Eigenvalue, type = "b", 
     xlab = "Component", ylab = "Eigenvalue",
     main = "Scree Plot of Eigenvalues",
     xlim = c(1, 11), ylim = c(0, max(plot_data$Eigenvalue)),
     pch = 19, col = "blue")

# Add grid lines for better readability
grid()

# Add points and values
for (i in 1:11) {
  points(i, plot_data$Eigenvalue[i], pch = 19, col = "blue")
  text(i, plot_data$Eigenvalue[i], labels = round(plot_data$Eigenvalue[i], 2), 
       pos = 3, cex = 0.8)
}

dev.off()
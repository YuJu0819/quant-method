create_plots <- function(x, var_name) {
    MU <- mean(x)
    SD <- sd(x)
    pt_x <- seq(from = min(x), to = max(x), length.out = length(x))
    pt_y <- dnorm(pt_x, mean = MU, sd = SD)

    png(paste0("./plots/", var_name, ".png"), width = 600, height = 1000)
    par(mfrow = c(2, 1))
    
    # Plot time series
    plot(time, x, type = "l", main = paste("Time Series of", var_name), 
        xlab = "Time", ylab = var_name)
    
    # Plot histogram with estimated normal density
    hist(x, freq = FALSE, main = paste("Histogram of", var_name, "with Normal Density"),
        xlab = var_name)
    lines(pt_x, pt_y, col = "red")
    
    dev.off()
}

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

# Q1. ===========================================================

# Create plots for all variables
variables <- list(y, x_dfy, x_infl, x_svar, x_tms, x_tbl, x_dfr, x_dp, x_ltr, x_ep, x_bmr, x_ntis)
var_names <- c("y", "x_dfy", "x_infl", "x_svar", "x_tms", "x_tbl", "x_dfr", "x_dp", "x_ltr", "x_ep", "x_bmr", "x_ntis")

for (i in 1:length(variables)) {
    create_plots(variables[[i]], var_names[i])
}

# Q2. ===========================================================

# Remove 'y' from the variables list and var_names
y <- variables[[1]]
variables <- variables[-1]
variables <- do.call(cbind, variables)

transposed_variables <- t(variables)
tmp_matrix <- transposed_variables %*% variables

result_matrix <- variables %*% solve(tmp_matrix) %*% transposed_variables
cat("Question 2-1: \n")
cat("The sum of the diagonal elements of the result matrix is: ", sum(diag(result_matrix)), "\n")

# Get the dimensions of the result matrix
n <- nrow(result_matrix)

# Create an identity matrix of the same size
identity_matrix <- diag(n)

# Subtract result_matrix from the identity matrix
final_matrix <- identity_matrix - result_matrix

cat("Question 2-2: \n")
cat("The sum of the diagonal elements of the final matrix is: ", sum(diag(final_matrix)), "\n")

# Q3. ===========================================================

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

# Q4. ===========================================================

col_avgs <- colMeans(variables)
col_stds <- apply(variables, 2, sd)
std_variables <- matrix(nrow = nrow(variables), ncol = ncol(variables))

for (i in 1:ncol(variables)) {
    for (j in 1:nrow(variables)) {
        std_variables[j, i] <- (variables[j, i] - col_avgs[i]) / col_stds[i]
    }
}

tmp_matrix <- t(std_variables) %*% std_variables
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
png("./plots/scree_plot_std1.png", width = 800, height = 600)

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

tmp_matrix <- std_variables %*% t(std_variables)
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
png("./plots/scree_plot_std2.png", width = 800, height = 600)

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

# Q5. ===========================================================

tmp_matrix <- t(std_variables) %*% std_variables
eigen_result <- eigen(tmp_matrix)
eigenvalues <- eigen_result$values
eigenvectors <- eigen_result$vectors

inverse_matrix <- eigenvectors %*% diag(1 / eigenvalues) %*% t(eigenvectors)

matrices_equal <- all.equal(inverse_matrix, solve(tmp_matrix), tolerance = 1e-6)
cat("check inverse result: ", matrices_equal, "\n")

identity_matrix <- inverse_matrix %*% solve(inverse_matrix)

matrices_equal <- all.equal(identity_matrix, diag(nrow(identity_matrix)), tolerance = 1e-6)
cat("check identity matrix: ", matrices_equal, "\n")

# Q6. ===========================================================
# Calculate by (X'X)^-1X'y
b <- solve(tmp_matrix) %*% t(std_variables) %*% y
# b <- solve(std_variables) %*% y
print("Result of b:")
print(b)
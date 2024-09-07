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
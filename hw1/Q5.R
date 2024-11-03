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
std_variables <- matrix(nrow = nrow(variables), ncol = ncol(variables))

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
eigenvectors <- eigen_result$vectors

inverse_matrix <- eigenvectors %*% diag(1 / eigenvalues) %*% t(eigenvectors)

matrices_equal <- all.equal(inverse_matrix, solve(tmp_matrix), tolerance = 1e-6)
cat("check inverse result: ", matrices_equal, "\n")

identity_matrix <- inverse_matrix %*% solve(inverse_matrix)

matrices_equal <- all.equal(identity_matrix, diag(nrow(identity_matrix)), tolerance = 1e-6)
cat("check identity matrix: ", matrices_equal, "\n")


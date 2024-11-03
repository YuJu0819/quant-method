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
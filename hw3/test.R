# Load necessary libraries
library(tidyverse)

# Read CSV data
dt <- read.csv("./Equity_Premium.csv")

# Convert Time column to Date format
dt$Time <- as.Date(paste0(dt$Time, "01"), format = "%Y%m%d")

# Define dependent variable (y) and independent variables (X)
X <- dt[, c("x_dfy", "x_infl", "x_svar", "x_tms", "x_tbl", "x_dfr")]
y <- dt$y

# Select 'x_tbl' as the variable of interest and the rest as control variables
X_interest <- X$x_tbl
X_controls <- X[, !colnames(X) %in% "x_tbl"]

# Step 1: Regress y on control variables (X_controls) and get residuals
lm_y_controls <- lm(y ~ ., data = as.data.frame(X_controls))
res_y_controls <- residuals(lm_y_controls)

# Step 2: Regress x_tbl on control variables (X_controls) and get residuals
lm_X_interest_controls <- lm(X_interest ~ ., data = as.data.frame(X_controls))
res_X_interest_controls <- residuals(lm_X_interest_controls)

# Step 3: Regress residuals of y on residuals of x_tbl
lm_residuals <- lm(res_y_controls ~ res_X_interest_controls)

# Display the results for FWL Theorem regression
summary(lm_residuals)

# Full model for comparison (regress y on both x_tbl and control variables)
lm_full <- lm(y ~ X_interest + ., data = as.data.frame(X_controls))
summary(lm_full)

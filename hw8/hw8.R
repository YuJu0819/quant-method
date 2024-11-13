# Load necessary libraries
library(ggplot2)   # For plotting

# Read the data
data <- read.csv("Equity_Premium.csv")

# Prepare the data
# Create squared terms
data$x_dfy2 <- data$x_dfy^2
data$x_infl2 <- data$x_infl^2
data$x_svar2 <- data$x_svar^2
data$x_tms2 <- data$x_tms^2
data$x_tbl2 <- data$x_tbl^2

# Define response and predictors
response <- "y"
predictors <- c("x_dfy", "x_infl", "x_svar", "x_tms", "x_tbl",
                "x_dfy2", "x_infl2", "x_svar2", "x_tms2", "x_tbl2")

# Add intercept as a predictor
all_predictors <- c("Intercept", predictors)
n_predictors <- length(all_predictors)

# Ensure there are no missing values
data <- na.omit(data)

# Number of observations
n <- nrow(data)

# Generate all possible combinations of predictors (2^11 = 2048 combinations)
predictor_list <- rep(list(c(0,1)), n_predictors)
combinations <- expand.grid(predictor_list)
colnames(combinations) <- all_predictors

# Total number of models
n_models <- nrow(combinations)

# Calculate Total Sum of Squares (TSS)
Y <- data[[response]]
TSS <- sum((Y - mean(Y))^2)

# Fit the full model to get sigma^2_hat_full
full_formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))
full_model <- lm(full_formula, data = data)
sigma2_hat_full <- sum(full_model$residuals^2) / (n - length(full_model$coefficients))

# Initialize a data frame to store results
results <- data.frame(model_number = 1:n_models,
                      predictors_included = character(n_models),
                      num_predictors = integer(n_models),
                      RSS = numeric(n_models),
                      R_squared = numeric(n_models),
                      adjusted_R_squared = numeric(n_models),
                      AIC = numeric(n_models),
                      BIC = numeric(n_models),
                      Mallows_Cp = numeric(n_models),
                      stringsAsFactors = FALSE
                      )


compute_LOOCV <- function(fit, data) {
  # Calculate the leverage values
  h <- hatvalues(fit)
  residuals <- residuals(fit)
  
  # Adjust residuals for LOO-CV
  PRESS <- sum((residuals / (1 - h))^2)
  LOOCV <- PRESS / n
  return(LOOCV)
}

# Loop over all possible models
for (i in 1:n_models) {
  included_predictors <- combinations[i, ]
  
  # Get the indices of predictors included (value is 1)
  included_indices <- which(included_predictors == 1)
  
  # If no predictors are included (intercept-only model or no intercept)
  if (length(included_indices) == 0) {
    formula <- as.formula(paste(response, "~ 0"))
    k <- 0
  } else {
    # Get the names of the predictors included
    predictors_in_model <- all_predictors[included_indices]
    
    # Check if intercept is included
    if ("Intercept" %in% predictors_in_model) {
      predictors_in_model <- setdiff(predictors_in_model, "Intercept")
      if (length(predictors_in_model) == 0) {
        formula <- as.formula(paste(response, "~ 1"))
      } else {
        formula <- as.formula(paste(response, "~", paste(predictors_in_model, collapse = "+")))
      }
    } else {
      # No intercept
      if (length(predictors_in_model) == 0) {
        formula <- as.formula(paste(response, "~ 0"))
      } else {
        formula <- as.formula(paste(response, "~ -1 +", paste(predictors_in_model, collapse = "+")))
      }
    }
  }
  
  # Fit the model
  fit <- lm(formula, data = data)
  
  # Number of parameters (k)
  k <- length(fit$coefficients)
  
  # Residual Sum of Squares (RSS)
  RSS <- sum(fit$residuals^2)
  
  # R-squared and Adjusted R-squared
  if (k == 0) {
    # No predictors
    R_squared <- 0
    adjusted_R_squared <- 0
  } else {
      R_squared <- 1 - RSS / TSS
      adjusted_R_squared <- 1 - ((RSS / (n - k)) / (TSS / (n - 1)))
  }
  
  # AIC and BIC
  AIC_value <- AIC(fit)
  BIC_value <- BIC(fit)
  
  # Mallows' Cp
  Cp <- (RSS) + 2 * sigma2_hat_full * k
  LOOCV <- compute_LOOCV(fit, data)
  # Store the results
  results$model_number[i] <- i
  results$predictors_included[i] <- ifelse(k == 0, "(None)",
                                           paste(setdiff(names(fit$coefficients), "(Intercept)"), collapse = ", "))
  results$num_predictors[i] <- k
  results$RSS[i] <- RSS
  results$R_squared[i] <- R_squared
  results$adjusted_R_squared[i] <- adjusted_R_squared
  results$AIC[i] <- AIC_value
  results$BIC[i] <- BIC_value
  results$Mallows_Cp[i] <- Cp
  results$LOO_CV[i] <- LOOCV
  
}

# Find the best models according to each criterion
best_R_squared_model <- results[which.max(results$R_squared), ]
best_adjusted_R_squared_model <- results[which.max(results$adjusted_R_squared), ]
best_AIC_model <- results[which.min(results$AIC), ]
best_BIC_model <- results[which.min(results$BIC), ]
results$Cp_diff <- abs(results$Mallows_Cp)
best_Cp_model <- results[which.min(results$Cp_diff), ]
best_LOO_CV_model <- results[which.min(results$LOO_CV), ]

# Print the best models
cat("Best model according to centered R^2:\n")
print(best_R_squared_model)

cat("\nBest model according to adjusted R^2:\n")
print(best_adjusted_R_squared_model)

cat("\nBest model according to AIC:\n")
print(best_AIC_model)

cat("\nBest model according to BIC:\n")
print(best_BIC_model)

cat("\nBest model according to Mallows' Cp:\n")
print(best_Cp_model)

cat("\nBest model according to LOO-CV:\n")
print(best_LOO_CV_model)


# Plotting model selection statistics
# Function to plot statistics
plot_statistic <- function(statistic, y_label) {
  ggplot(results, aes(x = num_predictors, y = statistic)) +
    geom_point(alpha = 0.5) +
    labs(title = paste(y_label, "vs Number of Predictors"), x = "Number of Predictors", y = y_label) +
    theme_minimal()
}

# Plot centered R^2
plot_statistic(results$R_squared, "Centered R^2")

# Plot adjusted R^2
plot_statistic(results$adjusted_R_squared, "Adjusted R^2")

# Plot AIC
plot_statistic(results$AIC, "AIC")

# Plot BIC
plot_statistic(results$BIC, "BIC")

# Plot Mallows' Cp
ggplot(results, aes(x = num_predictors, y = Mallows_Cp)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Mallows' Cp vs Number of Predictors", x = "Number of Predictors", y = "Mallows' Cp") +
  theme_minimal()

plot_statistic(results$LOO_CV, "LOO-CV Error")


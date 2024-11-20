# Load necessary libraries
library(tidyverse)
library(AER)       # For IV regression
library(stargazer) # For formatting tables
library(lmtest)



# Load the data
data <- readxl::read_excel("Card1995.xlsx")

# Drop rows with missing values in relevant columns

data <- data %>%
  mutate(across(where(is.character), ~ na_if(., "NA"))) %>% # Convert "NA" strings to NA
  filter(!is.na(wage76) & !is.na(lwage76) & !is.na(ed76)  & !is.na(nearc4) & !is.na(nearc4a) &  !is.na(nearc4b) & !is.na(age76) &  !is.na(smsa76r))   # Ensure relevant columns have no missing values

# colSums(is.na(data))
# Create necessary variables
data <- data %>%
  mutate(
    experience =  (age76-ed76 - 6),
    experience_sq = ((age76-ed76 - 6)^2) / 100 , # Adjust experience if it's defined differently
    age_sq = (age76^2)/100
  )
head(data)
# OLS Regression
ols_model <- lm(lwage76 ~ ed76 + experience + experience_sq + black + reg76r + smsa76r, data = data)

# IV(a): Instrument 1 (example: "nearc4")
iv_a_model <- ivreg(lwage76 ~ed76 + experience + experience_sq + black + reg76r + smsa76r| 
                    nearc4 + experience + experience_sq + black + reg76r + smsa76r, data = data)

# IV(b): Instrument 2 (example: "nearc2")
iv_b_model <- ivreg(lwage76 ~ ed76 + experience + experience_sq + black + reg76r + smsa76r| 
                    nearc4+ age76 + age_sq + black + reg76r + smsa76r, data = data)

# 2SLS(a): Similar instruments as IV(a)
sls_a_model <- ivreg(lwage76 ~ ed76 + experience + experience_sq + black + reg76r + smsa76r | 
                     nearc4a + nearc4b + experience + experience_sq + black + reg76r + smsa76r, data = data)

# 2SLS(b): Similar instruments as IV(b)
sls_b_model <- ivreg(lwage76 ~ed76 + experience + experience_sq + black + reg76r + smsa76r | 
                     nearc4a + nearc4b + age76  + age_sq + black + reg76r + smsa76r, data = data)

summary(sls_b_model)

# Extract residuals from the IV model
residuals_iv <- residuals(sls_b_model)

# Run a regression of residuals on the instruments
sargan_lm <- lm(residuals_iv ~  nearc4a + nearc4b + age76  + age_sq + black + reg76r + smsa76r, data = data)

# Compute the Sargan statistic
n <- nrow(data)  # Number of observations
r_squared <- summary(sargan_lm)$r.squared  # R-squared from the residual regression
sargan_statistic <- n * r_squared  # Sargan statistic

# Compute the degrees of freedom
df <- length(c("nearc4a", "nearc4b")) - 1  # Number of instruments - number of endogenous regressors

# Compute the p-value
p_value <- 1 - pchisq(sargan_statistic, df)

# Print the results
cat("Sargan Statistic:", sargan_statistic, "\n")
cat("P-value:", p_value, "\n")
# # Display results in a table

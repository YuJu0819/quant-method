# Load required libraries
# options(repos = c(CRAN = "https://cran.csie.ntu.edu.tw/"))
# install.packages("quantmod")
# install.packages("forecast")

library(tidyverse)
library(forecast)
library(lubridate)

process_covid_data <- function(data, value_name, cutoff_date = "2021-08-05") {
  data %>%
    # Ensure selection of relevant columns
    group_by(Country.Region) %>%
    select(Country.Region, starts_with("X")) %>%
    summarise(across(starts_with("X"), sum, na.rm = TRUE))  %>% 
    # Reshape from wide to long format
    pivot_longer(
      cols = starts_with("X"),
      names_to = "Date",
      values_to = value_name
    ) %>%
    # Convert date strings to proper Date format
    mutate(Date = as.Date(str_remove(Date, "X"), format = "%m.%d.%y")) %>%
    # Filter by cutoff date
    filter(Date < as.Date(cutoff_date)) %>%
    # Reshape from long back to wide format
    pivot_wider(
      names_from = Date,
      values_from = value_name
    )
}

get_group_ts <- function(data, group_name, frequency = 7) {
  data %>%
    filter(Group == group_name) %>%
    select(-Country.Region, -Group) %>%
    # 過濾可轉為數字的列
    mutate(across(where(is.character), as.numeric, .names = "num_{col}")) %>%
    select(where(is.numeric)) %>%
    replace(is.na(.), 0) %>% # 將 NA 替換為 0
    colSums(na.rm = TRUE) %>%
    ts(frequency = frequency)
}

add_group_classification <- function(data) {
  data %>%
    mutate(
      Group = case_when(
        Country.Region %in% developed ~ "Developed",
        Country.Region %in% undeveloped ~ "Undeveloped",
        TRUE ~ "Other"
      )
    )
}

# Load datasets
confirmed <- read.csv("time_series_covid19_confirmed_global.csv")
deaths <- read.csv("time_series_covid19_deaths_global.csv")
recovered <- read.csv("time_series_covid19_recovered_global.csv")

# Apply the processing function to each dataset
confirmed_grouped <- process_covid_data(confirmed, "Confirmed")
deaths_grouped <- process_covid_data(deaths, "Death")
recovered_grouped <- process_covid_data(recovered, "Recover")

country <- "Germany"
developed <- c("France", "Germany", "United Kingdom", "Japan", "Canada")
undeveloped <- c("Afghanistan", "Haiti", "Chad", "Somalia", "Nepal")

# Example: Time series for a specific country (e.g., US)
confirmed_grouped <- add_group_classification(confirmed_grouped)
deaths_grouped <- add_group_classification(deaths_grouped)
recovered_grouped <- add_group_classification(recovered_grouped)



confirmed_ts_developed <- get_group_ts(confirmed_grouped, "Developed")
confirmed_ts_undeveloped <- get_group_ts(confirmed_grouped, "Undeveloped")
print(confirmed_ts_developed)

deaths_ts_developed <- get_group_ts(deaths_grouped, "Developed")
deaths_ts_undeveloped <- get_group_ts(deaths_grouped, "Undeveloped")

recovered_ts_developed <- get_group_ts(recovered_grouped, "Developed")
recovered_ts_undeveloped <- get_group_ts(recovered_grouped, "Undeveloped")

# Example: Compare confirmed time series
plot.ts(confirmed_ts_developed, col = "blue", main = "Confirmed Cases: Developed vs. Undeveloped")
lines(confirmed_ts_undeveloped, col = "red")
legend("topright", legend = c("Developed", "Undeveloped"), col = c("blue", "red"), lty = 1)

# Cross-correlation to find lag between confirmed cases and deaths

ccf_d <- ccf(diff(deaths_ts), diff(confirmed_ts), lag.max = 30)
ccf_r <- ccf(diff(recovered_ts), diff(confirmed_ts))
plot(diff(confirmed_ts))
plot(diff(deaths_ts))
plot(diff(recovered_ts))

# Fit ARIMA model for deaths with lagged confirmed cases as exogenous variable
lag_confirmed <- stats::lag(confirmed_ts, lag = -2)  # Assuming a 7-day lag
arima_model <- auto.arima(deaths_ts, xreg = lag_confirmed)
summary(arima_model)

# Forecast deaths based on confirmed cases
forecast_result <- forecast(arima_model, xreg = lag_confirmed, h = 30)
autoplot(forecast_result)

# Time to Recovery Analysis
ccf_confirmed_recovered <- ccf(confirmed_ts, recovered_ts, lag.max = 30)
plot(ccf_confirmed_recovered)



# confirmed_ts <- confirmed_grouped %>%
#   filter(Country.Region == country) %>%
#   select(-Country.Region) %>%
#   t() %>%
#   as.numeric() %>%
#   ts(frequency = 7)

# deaths_ts <- deaths_grouped %>%
#   filter(Country.Region == country) %>%
#   select(-Country.Region) %>%
#   t() %>%
#   as.numeric() %>%
#   ts(frequency = 7)

# recovered_ts <- recovered_grouped %>%
#   filter(Country.Region == country) %>%
#   select(-Country.Region) %>%
#   t() %>%
#   as.numeric() %>%
#   ts(frequency = 7)
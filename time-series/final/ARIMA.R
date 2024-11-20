# Load required libraries
# options(repos = c(CRAN = "https://cran.csie.ntu.edu.tw/"))
# install.packages("quantmod")
# install.packages("forecast")
# Class A: High HDI Countries
developed <- c(
  "Albania",
  "Algeria",
  "Andorra",
  "Antigua and Barbuda",
  "Argentina",
  "Armenia",
  "Australia",
  "Austria",
  "Azerbaijan",
  "Bahamas",
  "Bahrain",
  "Barbados",
  "Belarus",
  "Belgium",
  "Bosnia and Herzegovina",
  "Brazil",
  "Brunei",
  "Bulgaria",
  "Canada",
  "Chile",
  "China",
  "Colombia",
  "Costa Rica",
  "Croatia",
  "Cuba",
  "Cyprus",
  "Czechia",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Georgia",
  "Germany",
  "Greece",
  "Hungary",
  "Iceland",
  "India",
  "Iran",
  "Ireland",
  "Israel",
  "Italy",
  "Japan",
  "Kazakhstan",
  "Korea, South",
  "Kuwait",
  "Latvia",
  "Lebanon",
  "Libya",
  "Lithuania",
  "Luxembourg",
  "Malaysia",
  "Malta",
  "Mauritius",
  "Mexico",
  "Montenegro",
  "Netherlands",
  "New Zealand",
  "Norway",
  "Oman",
  "Panama",
  "Poland",
  "Portugal",
  "Qatar",
  "Romania",
  "Russia",
  "Saudi Arabia",
  "Serbia",
  "Singapore",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "Switzerland",
  "Thailand",
  "Trinidad and Tobago",
  "Turkey",
  "Ukraine",
  "United Arab Emirates",
  "United Kingdom",
  "United States",
  "Uruguay"
)

# Class B: Low HDI Countries
undeveloped <- c(
  "Afghanistan",
  "Angola",
  "Bangladesh",
  "Belize",
  "Benin",
  "Bhutan",
  "Bolivia",
  "Botswana",
  "Burkina Faso",
  "Burundi",
  "Cabo Verde",
  "Cambodia",
  "Cameroon",
  "Central African Republic",
  "Chad",
  "Comoros",
  "Congo (Brazzaville)",
  "Congo (Kinshasa)",
  "Cote d'Ivoire",
  "Djibouti",
  "Dominica",
  "Dominican Republic",
  "Ecuador",
  "Egypt",
  "El Salvador",
  "Equatorial Guinea",
  "Eritrea",
  "Eswatini",
  "Ethiopia",
  "Fiji",
  "Gabon",
  "Gambia",
  "Ghana",
  "Grenada",
  "Guatemala",
  "Guinea",
  "Guinea-Bissau",
  "Guyana",
  "Haiti",
  "Honduras",
  "Iraq",
  "Jamaica",
  "Jordan",
  "Kenya",
  "Kiribati",
  "Kosovo",
  "Kyrgyzstan",
  "Laos",
  "Lesotho",
  "Liberia",
  "Madagascar",
  "Malawi",
  "Maldives",
  "Mali",
  "Marshall Islands",
  "Mauritania",
  "Micronesia",
  "Moldova",
  "Mongolia",
  "Morocco",
  "Mozambique",
  "Namibia",
  "Nauru",
  "Nepal",
  "Nicaragua",
  "Niger",
  "Nigeria",
  "North Macedonia",
  "Pakistan",
  "Palau",
  "Papua New Guinea",
  "Paraguay",
  "Peru",
  "Philippines",
  "Rwanda",
  "Saint Kitts and Nevis",
  "Saint Lucia",
  "Saint Vincent and the Grenadines",
  "Samoa",
  "San Marino",
  "Sao Tome and Principe",
  "Senegal",
  "Seychelles",
  "Sierra Leone",
  "Solomon Islands",
  "Somalia",
  "South Africa",
  "South Sudan",
  "Sri Lanka",
  "Sudan",
  "Suriname",
  "Syria",
  "Tajikistan",
  "Tanzania",
  "Timor-Leste",
  "Togo",
  "Tonga",
  "Tunisia",
  "Tuvalu",
  "Uganda",
  "Uzbekistan",
  "Vanuatu",
  "Venezuela",
  "Vietnam",
  "West Bank and Gaza",
  "Yemen",
  "Zambia",
  "Zimbabwe"
)

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


# Example: Time series for a specific country (e.g., US)
confirmed_grouped <- add_group_classification(confirmed_grouped)
deaths_grouped <- add_group_classification(deaths_grouped)
recovered_grouped <- add_group_classification(recovered_grouped)





confirmed_ts_developed <- get_group_ts(confirmed_grouped, "Developed")
confirmed_ts_undeveloped <- get_group_ts(confirmed_grouped, "Undeveloped")
# print(confirmed_ts_developed)

deaths_ts_developed <- get_group_ts(deaths_grouped, "Developed")
deaths_ts_undeveloped <- get_group_ts(deaths_grouped, "Undeveloped")

recovered_ts_developed <- get_group_ts(recovered_grouped, "Developed")
recovered_ts_undeveloped <- get_group_ts(recovered_grouped, "Undeveloped")

# Example: Compare confirmed time series
plot.ts(confirmed_ts_developed, col = "blue", main = "Confirmed Cases: Developed vs. Undeveloped")
lines(confirmed_ts_undeveloped, col = "red")
legend("topright", legend = c("Developed", "Undeveloped"), col = c("blue", "red"), lty = 1)

death_rate_dev <- (deaths_ts_developed / confirmed_ts_developed) * 100
recovery_rate_dev <- (recovered_ts_developed / confirmed_ts_developed) * 100

# Calculate death rate and recovery rate for undeveloped countries
death_rate_undeveloped <- (deaths_ts_undeveloped / confirmed_ts_undeveloped) * 100
recovery_rate_undeveloped <- (recovered_ts_undeveloped / confirmed_ts_undeveloped) * 100


# Ensure there are no division by zero errors
death_rate_dev[is.infinite(death_rate_dev) | is.nan(death_rate_dev)] <- 0
recovery_rate_dev[is.infinite(recovery_rate_dev) | is.nan(recovery_rate_dev)] <- 0
death_rate_undeveloped[is.infinite(death_rate_undeveloped) | is.nan(death_rate_undeveloped)] <- 0
recovery_rate_undeveloped[is.infinite(recovery_rate_undeveloped) | is.nan(recovery_rate_undeveloped)] <- 0

# Calculate confirmed growth rates for developed countries
differences_dev <- diff(confirmed_ts_developed)
previous_values_dev <- confirmed_ts_developed[-length(confirmed_ts_developed)]
growth_rates_dev <- (differences_dev / previous_values_dev) * 100
growth_rates_dev <- c(NA, growth_rates_dev)
growth_rates_ts_dev <- ts(growth_rates_dev, start = start(confirmed_ts_developed), frequency = frequency(confirmed_ts_developed))
growth_rates_ts_dev[is.infinite(growth_rates_ts_dev) | is.nan(growth_rates_ts_dev)] <- 0
# Calculate confirmed growth rates for undeveloped countries
differences_undeveloped <- diff(confirmed_ts_undeveloped)
previous_values_undeveloped <- confirmed_ts_undeveloped[-length(confirmed_ts_undeveloped)]
growth_rates_undeveloped <- (differences_undeveloped / previous_values_undeveloped) * 100
growth_rates_undeveloped <- c(NA, growth_rates_undeveloped)
growth_rates_ts_undeveloped <- ts(growth_rates_undeveloped, start = start(confirmed_ts_undeveloped), frequency = frequency(confirmed_ts_undeveloped))
growth_rates_ts_undeveloped[is.infinite(growth_rates_ts_undeveloped) | is.nan(growth_rates_ts_undeveloped)] <- 0
# Plotting the rates together for developed countries
par(mfrow = c(3, 1))  # Set up a 3-row plotting area

# Plot Confirmed Growth Rate
plot(growth_rates_ts_dev, type = "l", col = "blue",
     main = "Developed Countries: Confirmed Growth Rate",
     ylab = "Growth Rate (%)", xlab = "Time")

# Plot Death Rate
plot(death_rate_dev, type = "l", col = "red",
     main = "Developed Countries: Death Rate",
     ylab = "Death Rate (%)", xlab = "Time")

# Plot Recovery Rate
plot(recovery_rate_dev, type = "l", col = "green",
     main = "Developed Countries: Recovery Rate",
     ylab = "Recovery Rate (%)", xlab = "Time")

par(mfrow = c(1, 1))  # Reset plotting area

# Plotting the rates together for undeveloped countries
par(mfrow = c(3, 1))  # Set up a 3-row plotting area

# Plot Confirmed Growth Rate
plot(growth_rates_ts_undeveloped, type = "l", col = "blue",
     main = "Undeveloped Countries: Confirmed Growth Rate",
     ylab = "Growth Rate (%)", xlab = "Time")

# Plot Death Rate
plot(death_rate_undeveloped, type = "l", col = "red",
     main = "Undeveloped Countries: Death Rate",
     ylab = "Death Rate (%)", xlab = "Time")

# Plot Recovery Rate
plot(recovery_rate_undeveloped, type = "l", col = "green",
     main = "Undeveloped Countries: Recovery Rate",
     ylab = "Recovery Rate (%)", xlab = "Time")

par(mfrow = c(1, 1))  # Reset plotting area

# Optional: Plot all rates together for developed countries
plot(growth_rates_ts_dev, type = "l", col = "blue",
     ylim = range(c(growth_rates_ts_dev, death_rate_dev, recovery_rate_dev), na.rm = TRUE),
     main = "Developed Countries: Rates Comparison",
     ylab = "Rate (%)", xlab = "Time")
lines(death_rate_dev, col = "red")
lines(recovery_rate_dev, col = "green")
legend("topright", legend = c("Growth Rate", "Death Rate", "Recovery Rate"),
       col = c("blue", "red", "green"), lty = 1)

# Optional: Plot all rates together for undeveloped countries
plot(growth_rates_ts_undeveloped, type = "l", col = "blue",
     ylim = range(c(growth_rates_ts_undeveloped, death_rate_undeveloped, recovery_rate_undeveloped), na.rm = TRUE),
     main = "Undeveloped Countries: Rates Comparison",
     ylab = "Rate (%)", xlab = "Time")
lines(death_rate_undeveloped, col = "red")
lines(recovery_rate_undeveloped, col = "green")
legend("topright", legend = c("Growth Rate", "Death Rate", "Recovery Rate"),
       col = c("blue", "red", "green"), lty = 1)


ccf_d <- ccf(diff(deaths_ts_developed), diff(confirmed_ts_developed))
ccf_r <- ccf(diff(recovered_ts_developed), diff(confirmed_ts_developed))
ccd_d_u <- ccf(diff(deaths_ts_undeveloped), diff(confirmed_ts_undeveloped))
ccd_r_u <- ccf(diff(recovered_ts_undeveloped), diff(confirmed_ts_undeveloped))
plot(diff(confirmed_ts_developed))
plot(diff(deaths_ts_developed))
plot(diff(recovered_ts_developed))

# Fit ARIMA model for deaths with lagged confirmed cases as exogenous variable
lag_confirmed <- stats::lag(confirmed_ts_developed, lag = 2)  # Assuming a 7-day lag
arima_model <- auto.arima(deaths_ts_developed, xreg = lag_confirmed)
summary(arima_model)

# Forecast deaths based on confirmed cases
forecast_result <- forecast(arima_model, xreg = lag_confirmed, h = 30)
autoplot(forecast_result)

# Time to Recovery Analysis
ccf_confirmed_recovered <- ccf(confirmed_ts_developed, recovered_ts_developed, lag.max = 30)
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
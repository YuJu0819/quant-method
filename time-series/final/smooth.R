library(astsa)
library(ggplot2)
library(zoo)
library(forecast)

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

# developed <- c("United States", "United Kingdom")
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

smooth_ts <- function(ts_data, window = 7) {
  rollmean(ts_data, k = window, fill = 1, align = "right")
}
##################### Data Preperation #####################
# Load datasets
confirmed <- read.csv("time_series_covid19_confirmed_global.csv")
deaths <- read.csv("time_series_covid19_deaths_global.csv")
recovered <- read.csv("time_series_covid19_recovered_global.csv")
# Apply the processing function to each dataset
confirmed_grouped <- process_covid_data(confirmed, "Confirmed")
deaths_grouped <- process_covid_data(deaths, "Death")
recovered_grouped <- process_covid_data(recovered, "Recover")
# Example: Time series for a specific country (e.g., US)
confirmed_grouped <- add_group_classification(confirmed_grouped)
deaths_grouped <- add_group_classification(deaths_grouped)
recovered_grouped <- add_group_classification(recovered_grouped)

confirmed_ts_developed <- get_group_ts(confirmed_grouped, "Developed")
confirmed_ts_undeveloped <- get_group_ts(confirmed_grouped, "Undeveloped")

deaths_ts_developed <- get_group_ts(deaths_grouped, "Developed") 
deaths_ts_undeveloped <- get_group_ts(deaths_grouped, "Undeveloped")

recovered_ts_developed <- get_group_ts(recovered_grouped, "Developed")
recovered_ts_undeveloped <- get_group_ts(recovered_grouped, "Undeveloped")

confirmed_ts_developed_smooth <- smooth_ts(confirmed_ts_developed)
confirmed_ts_undeveloped_smooth <- smooth_ts(confirmed_ts_undeveloped)

# Apply smoothing to developed and undeveloped data for deaths
deaths_ts_developed_smooth <- smooth_ts(deaths_ts_developed)
deaths_ts_undeveloped_smooth <- smooth_ts(deaths_ts_undeveloped)

# Apply smoothing to developed and undeveloped data for recovered cases
recovered_ts_developed_smooth <- smooth_ts(recovered_ts_developed)
recovered_ts_undeveloped_smooth <- smooth_ts(recovered_ts_undeveloped)

acf2((diff(confirmed_ts_developed_smooth, difference = 3))) #done

acf2((diff(confirmed_ts_undeveloped_smooth, difference = 2)))#done

# trend <- lm(confirmed_ts_undeveloped ~ poly(time(confirmed_ts_undeveloped), degree = 6))
# detrended <- residuals(trend)
# plot(detrended)

acf2(diff(diff(deaths_ts_developed, difference = 3), lag = 7)) # D = 2
acf2(diff(deaths_ts_undeveloped, difference = 2))

acf2(diff(diff(recovered_ts_developed))) #done
acf2(diff(diff(recovered_ts_undeveloped, difference = 1), lag = 7)) #done


ccf_d <- ccf(smooth_ts(diff(diff(deaths_ts_developed, difference = 3), lag = 7)),smooth_ts( diff(diff(confirmed_ts_developed, difference = 3), lag = 7)))
ccf_r <- ccf(stats::lag((diff((recovered_ts_developed), difference = 2)), -8), diff(diff(confirmed_ts_developed, difference = 3), lag = 7)) #done

ccd_d_u <- ccf(stats::lag(smooth_ts(diff(deaths_ts_undeveloped, difference = 2), -8)), smooth_ts( diff(diff(confirmed_ts_developed, difference = 3), lag = 7)))
ccd_r_u <- ccf(stats::lag(smooth_ts(diff(diff(recovered_ts_undeveloped, difference = 1), lag = 7)), -2), smooth_ts( diff(diff(confirmed_ts_developed, difference = 3), lag = 7)))



# time <- 1:561
# model <- lm(deaths_ts_developed~ poly(time, 4, raw = TRUE))
# summary(model)
# death_d_resid <- residuals(model)

# acf2(diff(diff(death_d_resid, difference = 2), lag = 7))


# time <- 1:561
# model <- lm((confirmed_ts_undeveloped) ~ poly(time, 6) )


# confirmed_u_resid <- residuals(model)
# plot(confirmed_u_resid)
# acf2(diff(diff(confirmed_u_resid, difference = 3), lag = 7))
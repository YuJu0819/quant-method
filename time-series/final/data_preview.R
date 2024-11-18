# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)

# Load the dataset
file_path <- "time_series_covid19_confirmed_global.csv"
covid_data <- read.csv(file_path)

# Transform the data
covid_data_long <- covid_data %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Date",
               values_to = "Confirmed") %>%
  mutate(Date = mdy(str_replace(Date, "X", "")))

# Summarize by country
covid_country <- covid_data_long %>%
  group_by(Country.Region, Date) %>%
  summarize(Total_Confirmed = sum(Confirmed), .groups = 'drop')

# Select countries for comparison
selected_countries <- c("India", "Brazil", "Russia", "United Kingdom", "Cuba")
comparison_data <- covid_country %>%
  filter(Country.Region %in% selected_countries)

# Plot the time series trends
ggplot(comparison_data, aes(x = Date, y = Total_Confirmed, color = Country.Region)) +
  geom_line() +
  scale_y_log10() + # Log scale to handle large differences
  labs(title = "COVID-19 Confirmed Cases Over Time",
       x = "Date",
       y = "Total Confirmed Cases (Log Scale)",
       color = "Country") +
  theme_minimal()

# Additional analysis: Growth rates
comparison_data <- comparison_data %>%
  group_by(Country.Region) %>%
  arrange(Date) %>%
  mutate(Daily_Change = Total_Confirmed - lag(Total_Confirmed),
         Growth_Rate = Daily_Change / lag(Total_Confirmed))

# Plot growth rates
ggplot(comparison_data, aes(x = Date, y = Growth_Rate, color = Country.Region)) +
  geom_line() +
  labs(title = "COVID-19 Growth Rates Over Time",
       x = "Date",
       y = "Growth Rate",
       color = "Country") +
  theme_minimal()

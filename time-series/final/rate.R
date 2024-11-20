# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# Load datasets
confirmed_file <- "time_series_covid19_confirmed_global.csv"
deaths_file <- "time_series_covid19_deaths_global.csv"
recover_file <- "time_series_covid19_recovered_global.csv"

confirmed_data <- read.csv(confirmed_file)
deaths_data <- read.csv(deaths_file)
recover_data <- read.csv(recover_file)
# Aggregate data by country
confirmed_grouped <- confirmed_data %>%
  group_by(Country.Region) %>%
  summarise(across(starts_with("X"), sum, na.rm = TRUE))

deaths_grouped <- deaths_data %>%
  group_by(Country.Region) %>%
  summarise(across(starts_with("X"), sum, na.rm = TRUE))

recover_grouped <- recover_data %>%
  group_by(Country.Region) %>%
  summarise(across(starts_with("X"), sum, na.rm = TRUE))


# Calculate death rates (Deaths / Confirmed Cases * 100)
death_rates <- deaths_grouped %>%
  pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "Deaths") %>%
  inner_join(
    confirmed_grouped %>%
      pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "Confirmed"),
    by = c("Country.Region", "Date")
  ) %>%
  mutate(DeathRate = (Deaths / Confirmed) * 100,
         Date = mdy(str_remove(Date, "X"))) %>%
  filter(!is.na(DeathRate))


recover_rates <- recover_grouped %>%
  pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "Recovers") %>%
  inner_join(
    confirmed_grouped %>%
      pivot_longer(cols = starts_with("X"), names_to = "Date", values_to = "Confirmed"),
    by = c("Country.Region", "Date")
  ) %>%
  mutate(RecoverRate = (Recovers / Confirmed) * 100,
         Date = mdy(str_remove(Date, "X"))) %>%
  filter(!is.na(RecoverRate))


# Define country groupings
developed_countries <- c("US", "Germany", "Japan", "Australia", "Canada")
undeveloped_countries <- c("Afghanistan", "Haiti", "Chad", "Somalia", "Nepal")

# Filter data for the country groups
death_rates_grouped <- death_rates %>%
  mutate(Group = case_when(
    Country.Region %in% developed_countries ~ "Developed",
    Country.Region %in% undeveloped_countries ~ "Undeveloped",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Group)) %>%
  group_by(Group, Date) %>%
  summarise(MeanDeathRate = mean(DeathRate, na.rm = TRUE), .groups = "drop")

recover_rates_grouped <- recover_rates %>%
  mutate(Group = case_when(
    Country.Region %in% developed_countries ~ "Developed",
    Country.Region %in% undeveloped_countries ~ "Undeveloped",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Group)) %>%
  group_by(Group, Date) %>%
  summarise(MeanRecoverRate = mean(RecoverRate, na.rm = TRUE), .groups = "drop")

death_rates_grouped_filtered <- death_rates_grouped %>%
  filter(Date < as.Date("2021-08-05"))
recover_rates_grouped <- recover_rates_grouped %>% filter(Date <as.Date(("2021-08-05")))

# Plot comparison
ggplot(death_rates_grouped, aes(x = Date, y = MeanDeathRate, color = Group)) +
  geom_line(size = 1) +
  labs(title = "Death Rates Comparison Between Developed and Undeveloped Countries",
       x = "Date",
       y = "Death Rate (%)",
       color = "Country Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(recover_rates_grouped, aes(x = Date, y = MeanRecoverRate, color = Group)) +
  geom_line(size = 1) +
  labs(title = "Recover Rates Comparison Between Developed and Undeveloped Countries",
       x = "Date",
       y = "Recover Rate (%)",
       color = "Country Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

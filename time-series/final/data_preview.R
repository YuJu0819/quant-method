# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the CSV file (replace with your actual path)
data <- read_csv("./time_series_covid19_recovered_global.csv", show_col_types = FALSE)

# Filter data for Germany
germany_data <- data %>% filter(`Country/Region` == "Germany")

# Reshape the data from wide to long format, selecting date columns by excluding the first four columns
germany_data_long <- germany_data %>%
  pivot_longer(cols = -c(`Province/State`, `Country/Region`, Lat, Long), 
               names_to = "Date", values_to = "Recovered") %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))

# Plot the data
ggplot(germany_data_long, aes(x = Date, y = Recovered)) +
  geom_line() +
  labs(title = "COVID-19 Recovered Cases in Germany",
       x = "Date",
       y = "Recovered Cases") +
  theme_minimal()

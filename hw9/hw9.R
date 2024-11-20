# Load necessary library
library(tidyverse)

# Load the data
data <- readxl::read_excel("Card1995.xlsx")

# Create the transformed variables
data <- data %>%
  mutate(across(where(is.character), ~ na_if(., "NA")))
data <- data %>%
   filter(!is.na(wage76) & !is.na(lwage76))  # Remove rows with missing wage76 or lwage76
# print(data$lwage76)
data <- data %>%
  mutate(
    # log_wage = lwage76,  # Use the log wage for 1976
    experience =  (age76-ed76 - 6),
    experience_sq = ((age76-ed76 - 6)^2) / 100 , # Adjust experience if it's defined differently
    age_sq = (age76^2)/100
  )

# Model 1: log(wage) ~ experience + experience^2/100 + black + south + urban + college
model1 <- lm(lwage76 ~ experience + experience_sq + black + reg76r + smsa76r + nearc4, data = data)

# Model 2: education ~ experience + experience^2/100 + black + south + urban + college
model2 <- lm(ed76 ~ experience + experience_sq + black + reg76r + smsa76r + nearc4, data = data)

model3 <-  lm(ed76 ~ black + reg76r + smsa76r + nearc4 + age76 + age_sq, data = data) 
model4 <-  lm(experience ~ black + reg76r + smsa76r + nearc4 + age76 + age_sq, data = data) 
model5 <-  lm(experience_sq ~ black + reg76r + smsa76r + nearc4 + age76 + age_sq, data = data) 
model6 <-  lm(ed76 ~ experience + experience_sq + black + reg76r + smsa76r + nearc4a + nearc4b , data = data) 
# Print summaries
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)

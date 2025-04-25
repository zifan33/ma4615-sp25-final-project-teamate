library(haven)
library(lubridate)

data <- read_dta("dataset/data.dta")

data <- readRDS("dataset/data.rds")
data$year <- year(data$submitted_date)
data$month <- month(data$submitted_date)
data$day <- day(data$submitted_date)

## CLEAN DATA ##

# Check for missing values
colSums(is.na(data))
# Check for duplicates
sum(duplicated(data))
# Check for outliers in age_at_sub
summary(data$age_at_sub)
# Check unique values in race and gender
unique(data$race)
unique(data$gender)

# Standardize text columns
data <- data %>%
  mutate(
    race = tolower(trimws(race)),
    gender = tolower(trimws(gender)),
    state = toupper(trimws(state))
  ) %>%
  # Remove duplicates
  distinct(data)

# Save the cleaned data
saveRDS(data, "dataset/cleaned_data.rds")
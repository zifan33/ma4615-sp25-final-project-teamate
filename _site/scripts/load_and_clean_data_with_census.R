# load_and_clean_data_with_census.R
# Description: This script aggregates application-level data by state and year,
# merges it with census data to calculate the proportion of applications by state,
# and produces diagnostic maps to explore the relationship between callback rates and other variables.
#
# Data Sources:
# - Application data from the study: "Systemic Discrimination Among Large U.S. Employers" (Kline, Rose, and Walters, 2022)
# - Census data from the United States Census Bureau:
#   - https://www.census.gov/data/tables/time-series/demo/popest/2020s-state-total.html
#   - https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html

rm(list = ls())
options(warn = -1)
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(fixest))
suppressPackageStartupMessages(library(usmap))


data <- readRDS("dataset/cleaned_data.rds")
census_data <- read_excel('dataset/NST-EST2024-POP.xlsx')
census_data_2 <- read_excel('dataset/nst-est2019-01.xlsx', skip = 3)
census_data_2 <- census_data_2 %>%
  select(1, 13) %>%
  slice(6:56) 
colnames(census_data_2)[1] <- "states"
census_data_2 <- census_data_2 %>% 
  filter(grepl("^\\.", states)) %>%
  mutate(states = gsub("^\\.", "", states))

# --------------------------
data$full_name <- paste(data$firstname, data$lastname)
data$name_length <- nchar(data$full_name)


state_data <- data %>%
  group_by(state, year) %>%
  summarise(
    num_sub = n(),
    callback_rate = mean(cb),
    age = mean(age_at_sub),
    name_length = mean(name_length),
    proportion_black_name = sum(black == 1) / num_sub
  ) %>%
  ungroup()

print(unique(state_data$state))


colnames(census_data)[1] <- "states"
census_data <- census_data %>% 
  select(-2) %>% 
  slice(-(1:3))
colnames(census_data)[2:6] <- c("2020", "2021", "2022", "2023", "2024")


census_data <- census_data %>% filter(grepl("^\\.", states))
census_data <- census_data %>% 
  filter(states != ".Puerto Rico") %>%
  mutate(states = gsub("^\\.", "", states)) 

census_data <- census_data %>%
  left_join(census_data_2, by = "states")

census_data$state_abbr <- toupper(substr(census_data$states, 1, 2))

census_data_long <- census_data %>%
  pivot_longer(
    cols = -c(states, state_abbr),
    names_to = "year",
    values_to = "population",
    values_transform = list(population = as.numeric)
  ) %>%
  mutate(year = as.numeric(year))

print(unique(census_data$states))
print(unique(state_data$year))
print(unique(census_data_long$year))

state_mapping <- tibble::tribble(
  ~states,                     ~state_abbr_exp,
  "Alabama",                   "AL",
  "Alaska",                    "AK",
  "Arizona",                   "AZ",
  "Arkansas",                  "AR",
  "California",                "CA",
  "Colorado",                  "CO",
  "Connecticut",               "CT",
  "Delaware",                  "DE",
  "District of Columbia",      "DC",
  "Florida",                   "FL",
  "Georgia",                   "GA",
  "Hawaii",                    "HI",
  "Idaho",                     "ID",
  "Illinois",                  "IL",
  "Indiana",                   "IN",
  "Iowa",                      "IA",
  "Kansas",                    "KS",
  "Kentucky",                  "KY",
  "Louisiana",                 "LA",
  "Maine",                     "ME",
  "Maryland",                  "MD",
  "Massachusetts",             "MA",
  "Michigan",                  "MI",
  "Minnesota",                 "MN",
  "Mississippi",               "MS",
  "Missouri",                  "MO",
  "Montana",                   "MT",
  "Nebraska",                  "NE",
  "Nevada",                    "NV",
  "New Hampshire",             "NH",
  "New Jersey",                "NJ",
  "New Mexico",                "NM",
  "New York",                  "NY",
  "North Carolina",            "NC",
  "North Dakota",              "ND",
  "Ohio",                      "OH",
  "Oklahoma",                  "OK",
  "Oregon",                    "OR",
  "Pennsylvania",              "PA",
  "Rhode Island",              "RI",
  "South Carolina",            "SC",
  "South Dakota",              "SD",
  "Tennessee",                 "TN",
  "Texas",                     "TX",
  "Utah",                      "UT",
  "Vermont",                   "VT",
  "Virginia",                  "VA",
  "Washington",                "WA",
  "West Virginia",             "WV",
  "Wisconsin",                 "WI",
  "Wyoming",                   "WY"
)

census_data_long <- census_data_long %>%
  left_join(state_mapping, by = "states")

# --------------------------
# Merge Application and Census Data
# --------------------------
merged_data <- state_data %>%
  left_join(
    census_data_long %>% select(state_abbr_exp, year, population),
    by = c("state" = "state_abbr_exp", "year" = "year")
  )

cross_sectional <- merged_data %>%
  group_by(state) %>%
  summarise(
    num_sub = sum(num_sub),
    callback_rate = mean(callback_rate),
    age = mean(age),
    name_length = mean(name_length),
    proportion_black_name = sum(proportion_black_name * num_sub) / num_sub,
    population = mean(population),
    proportion_of_applicants = num_sub / population
  ) %>%
  ungroup()

# Map 1: Mean Population Size of Each State (2019-2021)
plot1 <- plot_usmap(data = cross_sectional, values = "population") +
  scale_fill_continuous(low = "white", high = "skyblue", 
                        name = "Population Size", 
                        label = scales::comma) +
  labs(title = "Mean Population Size of Each State 2019-2021") +
  theme(legend.position = "right")

# Map 2: Total Number of Applications Within Each State (2019-2021)
plot2 <- plot_usmap(data = cross_sectional, values = "num_sub") +
  scale_fill_continuous(low = "white", high = "skyblue", 
                        name = "Total Number of Applications", 
                        label = scales::comma) +
  labs(title = "Total Number of Applications Within State 2019-2021") +
  theme(legend.position = "right")

# Display the maps
print(plot1)
print(plot2)


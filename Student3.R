###########################################################################################################################################
#               Flight arrival delays are significantly influenced by airline operational efficiency, airport infrastructure capacity,
#                 seasonal/temporal factors, and external causes (e.g., weather, late aircraft, security).

# NAME:         STUDENT 1
# TP NUMBER:    TPXXXXX
# OBJECTIVE 1:  Perform data cleaning, preprocessing, and initial data exploration to prepare the dataset for analysis.
############################################################################################################################################

# Load necessary libraries
library(dplyr)
library(tidyr)
library(lubridate)  # For date/time handling

# Assume the dataset is loaded as 'df' from a CSV file
# df <- read.csv("path/to/flight_data.csv")  # Uncomment and set path if needed

# Initial data inspection
cat("\n=== INITIAL DATA INSPECTION ===\n")
cat("Dimensions:", dim(df), "\n")
cat("Column names:", names(df), "\n")
cat("Data types:\n")
str(df)

#------------------------------------------------------------------------
#                             DATA CLEANING
#------------------------------------------------------------------------
# STEP 1: Handle missing values
cat("\n=== MISSING VALUES SUMMARY ===\n")
missing_summary <- df %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  arrange(desc(Missing_Count))
print(missing_summary)

# For delay columns, replace NA with 0 (assuming NA means no delay)
delay_cols <- c("AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")
df <- df %>%
  mutate(across(all_of(delay_cols), ~ifelse(is.na(.), 0, .)))

# For other critical columns, remove rows with NA if few
# Example: Remove rows with NA in key columns like ARRIVAL_DELAY, AIRLINE, etc.
df <- df %>%
  filter(!is.na(ARRIVAL_DELAY), !is.na(AIRLINE), !is.na(ORIGIN_AIRPORT))

# STEP 2: Handle duplicates
cat("\n=== DUPLICATE CHECK ===\n")
cat("Number of duplicate rows:", sum(duplicated(df)), "\n")
df <- distinct(df)  # Remove duplicates

# STEP 3: Transform time variables
# Convert SCHEDULED_DEPARTURE and SCHEDULED_ARRIVAL to proper time formats if needed
# Assuming they are in HHMM format
df <- df %>%
  mutate(
    SCHEDULED_DEPARTURE_TIME = sprintf("%04d", SCHEDULED_DEPARTURE),
    SCHEDULED_DEPARTURE_HOUR = as.integer(substr(SCHEDULED_DEPARTURE_TIME, 1, 2)),
    SCHEDULED_DEPARTURE_MIN = as.integer(substr(SCHEDULED_DEPARTURE_TIME, 3, 4))
  )

# Similarly for arrival
if ("SCHEDULED_ARRIVAL" %in% names(df)) {
  df <- df %>%
    mutate(
      SCHEDULED_ARRIVAL_TIME = sprintf("%04d", SCHEDULED_ARRIVAL),
      SCHEDULED_ARRIVAL_HOUR = as.integer(substr(SCHEDULED_ARRIVAL_TIME, 1, 2)),
      SCHEDULED_ARRIVAL_MIN = as.integer(substr(SCHEDULED_ARRIVAL_TIME, 3, 4))
    )
}

# Convert MONTH, DAY, YEAR to Date if possible
# Assuming YEAR, MONTH, DAY are present
if (all(c("YEAR", "MONTH", "DAY") %in% names(df))) {
  df <- df %>%
    mutate(DATE = make_date(YEAR, MONTH, DAY))
}

# STEP 4: Outlier handling (basic)
# For arrival delay, cap extreme outliers
df <- df %>%
  mutate(ARRIVAL_DELAY = ifelse(ARRIVAL_DELAY > 1000, 1000, ARRIVAL_DELAY))  # Cap at 1000 min

# STEP 5: Data type corrections
# Ensure categorical variables are factors
df <- df %>%
  mutate(
    AIRLINE = as.factor(AIRLINE),
    ORIGIN_AIRPORT = as.factor(ORIGIN_AIRPORT),
    DESTINATION_AIRPORT = as.factor(DESTINATION_AIRPORT),
    MONTH = as.factor(MONTH),
    DAY_OF_WEEK = as.factor(DAY_OF_WEEK)
  )

# Final cleaned data summary
cat("\n=== CLEANED DATA SUMMARY ===\n")
cat("Dimensions after cleaning:", dim(df), "\n")
cat("Missing values after cleaning:\n")
print(colSums(is.na(df)))

# Save cleaned data if needed
# write.csv(df, "cleaned_flight_data.csv")  # Uncomment to save
###########################################################################################################################################
#               Flight arrival delays are significantly influenced by airline operational efficiency, airport infrastructure capacity,
#                 seasonal/temporal factors, and external causes (e.g., weather, late aircraft, security).

# NAME:         STUDENT 2
# TP NUMBER:    TPXXXXX
# OBJECTIVE 2:  Perform basic statistical analysis and exploratory data analysis to understand the dataset structure and key variables.
############################################################################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)

# Assume df is the cleaned dataset from Objective 1

#------------------------------------------------------------------------
#                             BASIC STATISTICS
#------------------------------------------------------------------------
# STEP 1: Summary statistics for key variables
cat("\n=== SUMMARY STATISTICS ===\n")
key_vars <- c("ARRIVAL_DELAY", "DEPARTURE_DELAY", "DISTANCE", "AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")
summary_stats <- df %>%
  select(all_of(key_vars[key_vars %in% names(df)])) %>%
  summary()
print(summary_stats)

# STEP 2: Flight counts by categories
cat("\n=== FLIGHT COUNTS ===\n")
cat("Total flights:", nrow(df), "\n")
cat("Flights by airline:\n")
print(table(df$AIRLINE))
cat("\nFlights by month:\n")
print(table(df$MONTH))
cat("\nFlights by day of week:\n")
print(table(df$DAY_OF_WEEK))

# STEP 3: Delay statistics
cat("\n=== DELAY STATISTICS ===\n")
delayed_flights <- sum(df$ARRIVAL_DELAY > 0, na.rm = TRUE)
cat("Number of delayed flights:", delayed_flights, "\n")
cat("Percentage of delayed flights:", round(delayed_flights / nrow(df) * 100, 2), "%\n")
cat("Average delay for delayed flights:", mean(df$ARRIVAL_DELAY[df$ARRIVAL_DELAY > 0], na.rm = TRUE), "minutes\n")

#------------------------------------------------------------------------
#                             EXPLORATORY DATA ANALYSIS
#------------------------------------------------------------------------
# STEP 4: Distribution of arrival delays
ggplot(df, aes(x = ARRIVAL_DELAY)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  xlim(-50, 200) +
  labs(title = "Distribution of Arrival Delays", x = "Arrival Delay (minutes)", y = "Frequency") +
  theme_minimal()

# STEP 5: Boxplot of delays by airline
top_airlines <- df %>%
  group_by(AIRLINE) %>%
  summarise(count = n()) %>%
  top_n(10, count) %>%
  pull(AIRLINE)

df %>%
  filter(AIRLINE %in% top_airlines) %>%
  ggplot(aes(x = reorder(AIRLINE, ARRIVAL_DELAY, median), y = ARRIVAL_DELAY)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Arrival Delays by Airline", x = "Airline", y = "Arrival Delay (minutes)") +
  theme_minimal()

# STEP 6: Delays by month
df %>%
  group_by(MONTH) %>%
  summarise(mean_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>%
  ggplot(aes(x = MONTH, y = mean_delay, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "Average Arrival Delay by Month", x = "Month", y = "Mean Delay (minutes)") +
  theme_minimal()

# STEP 7: Correlation matrix for delay variables
delay_vars <- c("ARRIVAL_DELAY", "DEPARTURE_DELAY", "AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")
delay_vars_present <- delay_vars[delay_vars %in% names(df)]

if (length(delay_vars_present) > 1) {
  cor_matrix <- cor(df %>% select(all_of(delay_vars_present)), use = "complete.obs")
  cat("\n=== CORRELATION MATRIX ===\n")
  print(round(cor_matrix, 2))
}

# STEP 8: Scatter plot: Distance vs Delay
ggplot(df, aes(x = DISTANCE, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Arrival Delay vs Flight Distance", x = "Distance", y = "Arrival Delay (minutes)") +
  theme_minimal()

# STEP 9: Proportion of delay causes
delay_cols <- c("AIR_SYSTEM_DELAY", "SECURITY_DELAY", "AIRLINE_DELAY", "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY")
delay_cols_present <- delay_cols[delay_cols %in% names(df)]

if (length(delay_cols_present) > 0) {
  delay_sums <- df %>%
    summarise(across(all_of(delay_cols_present), ~sum(. > 0, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Cause", values_to = "Count") %>%
    mutate(Percentage = Count / sum(Count) * 100)

  ggplot(delay_sums, aes(x = reorder(Cause, -Percentage), y = Percentage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Proportion of Flights with Each Delay Cause", x = "Delay Cause", y = "Percentage") +
    theme_minimal() +
    coord_flip()
}

#-----------------------------------------------------------------------------
#CONCLUSION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cat("\n=== KEY INSIGHTS ===\n")
cat("- Dataset dimensions:", dim(df), "\n")
cat("- Most common airlines:", names(sort(table(df$AIRLINE), decreasing = TRUE)[1:3]), "\n")
cat("- Average arrival delay:", round(mean(df$ARRIVAL_DELAY, na.rm = TRUE), 2), "minutes\n")
cat("- Percentage delayed:", round(mean(df$ARRIVAL_DELAY > 0, na.rm = TRUE) * 100, 2), "%\n")
###########################################################################################################################################
#               Flight arrival delays are significantly influenced by airline operational efficiency, airport infrastructure capacity, 
#                 seasonal/temporal factors, and external causes (e.g., weather, late aircraft, security). 

# NAME:         STUDENT 3
# TP NUMBER:    TPXXXXX
# OBJECTVE 3:   Investigate how time factors (month, day of week, departure hour) affect arrival delays,
#               and how these temporal patterns interact with airline efficiency and airport capacity.
############################################################################################################################################

library(ggplot2)
library(ggridges)


# Flights per month / day of week
counts_month <- df %>% count(MONTH, sort = TRUE)
counts_dow   <- df %>% count(DAY_OF_WEEK, sort = TRUE)
cat("\n=== FLIGHT COUNTS BY MONTH ===\n"); print(counts_month)
cat("\n=== FLIGHT COUNTS BY DAY_OF_WEEK (1=Mon..7=Sun, typical coding) ===\n"); print(counts_dow)

# Average arrival delay by month
df %>%
  group_by(MONTH) %>%
  summarise(mean_arr_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(MONTH), y = mean_arr_delay, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "Mean Arrival Delay by Month",
       x = "Month", y = "Mean Arrival Delay (min)") +
  theme_minimal()

# (Optional) Approximate scheduled departure hour from SCHEDULED_DEPARTURE (HHMM int)
# This is *exploratory only* (no mutation of the master df)
df_hour <- df %>%
  select(SCHEDULED_DEPARTURE, ARRIVAL_DELAY) %>%
  filter(!is.na(SCHEDULED_DEPARTURE)) %>%
  mutate(
    HHMM = sprintf("%04d", as.integer(SCHEDULED_DEPARTURE)),
    HOUR = as.integer(substr(HHMM, 1, 2))
  )

df_hour %>%
  group_by(HOUR) %>%
  summarise(mean_arr_delay = mean(ARRIVAL_DELAY, na.rm = TRUE),
            n = n()) %>%
  ggplot(aes(x = HOUR, y = mean_arr_delay)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Mean Arrival Delay by Scheduled Departure Hour (approx.)",
       x = "Scheduled Departure Hour", y = "Mean Arrival Delay (min)") +
  theme_minimal()


#------------------------------------------------------------------------
#                             ANALYSIS
#------------------------------------------------------------------------
# STEP 1: TEMPORAL PATTERNS ALONE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df %>%
  group_by(MONTH, DAY_OF_WEEK) %>%
  summarise(mean_arr_delay = mean(ARRIVAL_DELAY, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = MONTH, y = DAY_OF_WEEK, size = mean_arr_delay, fill = mean_arr_delay)) +
  geom_point(shape = 21, color = "black") +
  scale_size_continuous(range = c(2,12)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Balloon Plot of Mean Arrival Delay (Month × Day of Week)",
       x = "Month", y = "Day of Week", size = "Mean Delay", fill = "Mean Delay") +
  theme_minimal() 



# Ridgeline plot: Distribution by Departure Hour
df %>%
  mutate(DEP_HOUR = ifelse(!is.na(SCHEDULED_DEPARTURE), floor(SCHEDULED_DEPARTURE/100), NA)) %>%
  ggplot(aes(x = ARRIVAL_DELAY, y = factor(DEP_HOUR), fill = factor(DEP_HOUR))) +
  geom_density_ridges(alpha = 0.6, scale = 2) +
  coord_cartesian(xlim = c(-50, 300)) +
  labs(title = "Ridgeline Plot of Arrival Delays by Departure Hour",
       x = "Arrival Delay (minutes)", y = "Departure Hour") +
  theme_ridges() +
  theme(legend.position = "none") 

df %>%
  group_by(MONTH) %>%
  summarise(
    flights = n(),
    mean_delay = mean(ARRIVAL_DELAY, na.rm = TRUE),
    sd_delay = sd(ARRIVAL_DELAY, na.rm = TRUE)
  )

# ANOVA across months
anova_month <- aov(ARRIVAL_DELAY ~ MONTH, data = df)
summary(anova_month)
#------------------------------------------------------------------------
# STEP 2: TEMPORAL PATTERNS X AIRLINE EFFICIENCY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df %>%
  group_by(AIRLINE, MONTH) %>%
  summarise(mean_arr_delay = mean(ARRIVAL_DELAY, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = MONTH, y = mean_arr_delay, group = AIRLINE, color = AIRLINE)) +
  geom_line() + geom_point() +
  facet_wrap(~ AIRLINE) +
  labs(title = "Monthly Delay Trends by Airline",
       x = "Month", y = "Mean Arrival Delay (minutes)") +
  theme_minimal()

anova_airline_month <- aov(ARRIVAL_DELAY ~ AIRLINE * MONTH, data = df)
summary(anova_airline_month)

#------------------------------------------------------------------------
# STEP 3: TEMPORAL PATTERNS X AIRPORT CAPACITY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df %>%
  mutate(DEP_HOUR = ifelse(!is.na(SCHEDULED_DEPARTURE), floor(SCHEDULED_DEPARTURE/100), NA)) %>%
  group_by(ORIGIN_AIRPORT, DEP_HOUR) %>%
  summarise(mean_arr_delay = mean(ARRIVAL_DELAY, na.rm = TRUE), .groups = "drop") %>%
  filter(ORIGIN_AIRPORT %in% c("ATL","ORD","DFW","LAX","DEN")) %>%  # example top 5 airports
  ggplot(aes(x = DEP_HOUR, y = ORIGIN_AIRPORT, fill = mean_arr_delay)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heatmap of Mean Delay by Airport × Departure Hour",
       x = "Departure Hour", y = "Origin Airport", fill = "Mean Delay") +
  theme_minimal() 

df <- df %>%
  mutate(DEP_HOUR = ifelse(!is.na(SCHEDULED_DEPARTURE),
                           floor(SCHEDULED_DEPARTURE / 100), NA))

anova_airport_hour <- aov(ARRIVAL_DELAY ~ ORIGIN_AIRPORT * DEP_HOUR, data = df)
summary(anova_airport_hour)
#------------------------------------------------------------------------
# STEP 4: ADDING EXTERNAL CAUSES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
delay_cols <- c("AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY","LATE_AIRCRAFT_DELAY","WEATHER_DELAY")

df %>%
  group_by(MONTH) %>%
  summarise(across(all_of(delay_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(delay_cols), names_to = "Cause", values_to = "AvgMinutes") %>%
  ggplot(aes(x = MONTH, y = AvgMinutes, fill = Cause)) +
  geom_area(position = "stack", alpha = 0.8) +
  labs(title = "Monthly Trends of Delay Causes",
       x = "Month", y = "Average Delay Minutes", fill = "Cause") +
  theme_minimal()


# Example: Kruskal-Wallis pour le delai mensuel
kruskal.test(WEATHER_DELAY ~ MONTH, data = df)

# Correlation entre les causes du delai pour departure et delai de l'arrivée
delay_cols <- c("AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY",
                "LATE_AIRCRAFT_DELAY","WEATHER_DELAY")

cor_matrix <- cor(df %>% select(ARRIVAL_DELAY, all_of(delay_cols)), use = "complete.obs")
round(cor_matrix, 2)
#-----------------------------------------------------------------------------
#CONCLUSION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- df %>%
  mutate(Weekend = ifelse(DAY_OF_WEEK %in% c("Sat","Sun"), "Weekend", "Weekday"))

wilcox.test(ARRIVAL_DELAY ~ Weekend, data = df)

df %>%
  group_by(MONTH) %>%
  summarise(mean_arr_delay = mean(ARRIVAL_DELAY, na.rm = TRUE)) %>%
  lm(mean_arr_delay ~ as.numeric(MONTH), data = .) %>%
  summary()

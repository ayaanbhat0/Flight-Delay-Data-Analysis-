###########################################################################################################################################
#               Flight arrival delays are significantly influenced by airline operational efficiency, airport infrastructure capacity, 
#                 seasonal/temporal factors, and external causes (e.g., weather, late aircraft, security).
# NAME:         STUDENT 4
# TP NUMBER:    TPXXXXX
# OBJECTIVE 4 : To evaluate the effect of external delay factors (weather, late aircraft, security, air system) on arrival delays
############################################################################################################################################ 
install.packages("GGally")
install.packages("streamgraph")

library(fmsb)
library(GGally)
library(streamgraph)
# Summaries of delay categories (may be NA if no delay)
delay_cols <- c("AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY","LATE_AIRCRAFT_DELAY","WEATHER_DELAY")
present_cols <- delay_cols[delay_cols %in% names(df)]

if (length(present_cols) > 0) {
  cat("\n=== EXTERNAL DELAY COMPONENTS SUMMARY ===\n")
  comp_summary <- df %>%
    summarise(across(all_of(present_cols),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          median = ~median(.x, na.rm = TRUE),
                          pct_nonmissing = ~mean(!is.na(.x))*100),
                     .names = "{.col}_{.fn}"))
  print(comp_summary)
  
  # Stacked bar (share of non-missing delay components among delayed flights)
  comp_long <- df %>%
    select(ARRIVAL_DELAY, all_of(present_cols)) %>%
    filter(!is.na(ARRIVAL_DELAY) & ARRIVAL_DELAY > 0) %>%
    pivot_longer(cols = all_of(present_cols), names_to = "component", values_to = "minutes")
  
  comp_long %>%
    mutate(has_component = !is.na(minutes)) %>%
    count(component, has_component) %>%
    group_by(component) %>%
    mutate(pct = n / sum(n)) %>%
    filter(has_component) %>%
    ggplot(aes(x = reorder(component, -pct), y = pct)) +
    geom_col() +
    scale_y_continuous(labels = percent) +
    labs(title = "Share of Delayed Flights with Each Delay Component Reported",
         x = "Delay Component", y = "Percent of delayed flights") +
    theme_minimal()
} 

#------------------------------------------------------------------------
#                             ANALYSIS
#------------------------------------------------------------------------
# STEP 1: External causes alone
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summary stats for delay causes
delay_cols <- c("AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY",
                "LATE_AIRCRAFT_DELAY","WEATHER_DELAY")

df %>%
  summarise(across(all_of(delay_cols),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE))))

# Correlation with arrival delay
cor_matrix <- cor(df %>% select(ARRIVAL_DELAY, all_of(delay_cols)), use = "complete.obs")
round(cor_matrix, 2)

# Prepare mean values of causes
cause_means <- df %>%
  summarise(across(c(AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY,
                     LATE_AIRCRAFT_DELAY, WEATHER_DELAY), ~mean(.x, na.rm = TRUE)))

# Radar chart needs max/min rows
radar_data <- rbind(rep(max(cause_means), 5), rep(0, 5), cause_means)
rownames(radar_data) <- c("Max","Min","Average")

radarchart(radar_data, axistype=1,
           pcol="red", pfcol=rgb(0.2,0.5,0.5,0.5), plwd=3,
           title="Radar Chart of Average Delay Causes")

#------------------------------------------------------------------------
# STEP 2: Causes X Airline Efficiency
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Average cause minutes by airline
df %>%
  group_by(AIRLINE) %>%
  summarise(across(all_of(delay_cols), ~mean(.x, na.rm = TRUE)))

# Example: Kruskal–Wallis for weather delay across airlines
kruskal.test(WEATHER_DELAY ~ AIRLINE, data = df)

df %>%
  group_by(AIRLINE) %>%
  summarise(across(c(AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY,
                     LATE_AIRCRAFT_DELAY, WEATHER_DELAY), ~mean(.x, na.rm = TRUE))) %>%
  GGally::ggparcoord(columns = 2:6, groupColumn = 1, scale = "globalminmax") +
  labs(title = "Parallel Coordinates: Delay Causes Across Airlines",
       x = "Cause", y = "Avg Delay Minutes") +
  theme_minimal()
#------------------------------------------------------------------------
# STEP 3: Causes X airport capacity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Average cause minutes by airport
df %>%
  group_by(ORIGIN_AIRPORT) %>%
  summarise(across(all_of(delay_cols), ~mean(.x, na.rm = TRUE)))

# Convert to binary (cause present or not)
df_binary <- df %>%
  mutate(across(all_of(delay_cols), ~ifelse(. > 0, 1, 0)))

chisq.test(table(df_binary$ORIGIN_AIRPORT, df_binary$WEATHER_DELAY))
# Binary presence of weather delay
df_binary <- df %>%
  mutate(Weather_Flag = ifelse(WEATHER_DELAY > 0, "Yes", "No"))

mosaicplot(table(df_binary$ORIGIN_AIRPORT, df_binary$Weather_Flag),
           main="Mosaic Plot of Weather Delay by Airport",
           xlab="Origin Airport", ylab="Weather Delay Present")

#------------------------------------------------------------------------
# STEP 4: Causes X Temporal Patterns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Example: Kruskal–Wallis for airline delay by month
kruskal.test(AIRLINE_DELAY ~ MONTH, data = df)

# Trend regression for weather delays across months
df %>%
  group_by(MONTH) %>%
  summarise(mean_weather = mean(WEATHER_DELAY, na.rm = TRUE)) %>%
  lm(mean_weather ~ as.numeric(MONTH), data = .) %>%
  summary()

df %>%
  group_by(MONTH) %>%
  summarise(across(c(AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY,
                     LATE_AIRCRAFT_DELAY, WEATHER_DELAY), ~mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-MONTH, names_to="Cause", values_to="AvgMinutes") %>%
  streamgraph("Cause", "AvgMinutes", "MONTH") %>%
  sg_axis_x(tick_format="d") %>%
  sg_fill_tableau()
#-----------------------------------------------------------------------------
#CONCLUSION
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lm_model <- lm(ARRIVAL_DELAY ~ AIR_SYSTEM_DELAY + SECURITY_DELAY +
                 AIRLINE_DELAY + LATE_AIRCRAFT_DELAY + WEATHER_DELAY, data=df)
summary(lm_model)
cor(df$ARRIVAL_DELAY, df$WEATHER_DELAY, method="spearman", use="complete.obs")
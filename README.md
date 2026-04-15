# Flight Delay Data Analysis

This project analyzes an aviation flight-delay dataset using R for data cleaning, preprocessing, and statistical analysis. The analysis explores delay patterns to evaluate airline performance, airport infrastructure capacity, seasonal/temporal factors, and external causes affecting flight efficiency.

## Objectives

The project is divided into multiple objectives, each handled by different team members:

1. **Objective 1** (Student 1): Perform data cleaning, preprocessing, and initial data exploration to prepare the dataset for analysis.
2. **Objective 2** (Student 2): Perform basic statistical analysis and exploratory data analysis to understand the dataset structure and key variables.
3. **Objective 3** (Student 3): Investigate how time factors (month, day of week, departure hour) affect arrival delays, and how these temporal patterns interact with airline efficiency and airport capacity.
4. **Objective 4** (Student 4): Evaluate the effect of external delay factors (weather, late aircraft, security, air system) on arrival delays.

## Files

- `Student3.R`: Script for Objective 1. Handles missing values, duplicates, time transformations, and data type corrections.
- `Student2.R`: Script for Objective 2. Provides summary statistics, distributions, and basic visualizations.
- `Student1.R`: Script for Objective 3. Analyzes temporal patterns, interactions with airlines and airports, using plots like ridgeline, heatmaps, and ANOVA.
- `EDA.R`: Main exploratory data analysis script for Objective 4. Includes summaries, correlations, visualizations (radar charts, parallel coordinates, streamgraphs), and statistical tests.

## Dependencies

The scripts require R and the following packages:

- `ggplot2`
- `dplyr`
- `tidyr`
- `ggridges`
- `GGally`
- `streamgraph`
- `fmsb`
- `lubridate`

Install them using:

```r
install.packages(c("ggplot2", "dplyr", "tidyr", "ggridges", "GGally", "streamgraph", "fmsb", "lubridate"))
```

## Usage

1. Load the flight delay dataset into a data frame named `df`. (The dataset is not included in this repository; ensure you have access to the aviation flight-delay data with columns like ARRIVAL_DELAY, AIRLINE, ORIGIN_AIRPORT, MONTH, etc.)

2. Run the R scripts in RStudio or R console in the following order. Each script assumes `df` is available in the environment.

3. Execute `Student3.R` for Objective 1: Data cleaning and preprocessing.

4. Execute `Student2.R` for Objective 2: Basic statistical analysis and EDA.

5. Execute `Student1.R` for Objective 3: Temporal analysis.

6. Execute `EDA.R` for Objective 4: External delay factors analysis.

## Data

The analysis uses a dataset of flight records with variables including:
- Arrival and departure delays
- Airline and airport codes
- Temporal variables (month, day of week, scheduled times)
- Delay causes (weather, security, airline, late aircraft, air system)

## Contributors

- Student 1 (TPXXXXX): Objective 1
- Student 2 (TPXXXXX): Objective 2
- Student 3 (TPXXXXX): Objective 3
- Student 4 (TPXXXXX): Objective 4

## License

This project is for educational purposes.

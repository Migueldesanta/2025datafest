library(readr)
library(dplyr)
library(tidyr)
library(scales)

# Step 1: Load processed feature data
features <- read_csv("C:/Users/yunxi/Desktop/Dataset 2025/feature/features.csv")

# Step 2: Select and clean relevant columns
selected <- features %>%
  select(
    year, quarter, market,
    log_total_leased_sf,
    log_overall_rent,
    availability_proportion,
    avg_occupancy_proportion,
    unemployment_rate
  ) %>%
  drop_na()

# Step 3: Z-score standardization
z_data <- selected %>%
  mutate(across(
    c(log_total_leased_sf, log_overall_rent, availability_proportion,
      avg_occupancy_proportion, unemployment_rate),
    ~ scale(.)[, 1],
    .names = "z_{.col}"
  ))

# Step 4: Compute trend_score as mean of z-scores
z_data <- z_data %>%
  mutate(trend_score = rowMeans(select(., starts_with("z_"))))

# Step 5: Export
write_csv(z_data, "trend_scores.csv")

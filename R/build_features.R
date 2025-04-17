# -------------------------------
# Feature Engineering Script
# -------------------------------
library(dplyr)
library(readr)
library(lubridate)
library(mice)

# Step 1: Load datasets
leases        <- read_csv("data/raw/Leases.csv")
rent          <- read_csv("data/raw/Price and Availability Data.csv")
occupancy     <- read_csv("data/raw/Major Market Occupancy Data.csv")
unemployment  <- read_csv("data/raw/Unemployment.csv")

# Step 2: Filter leases for ≥ 10,000 SF and year ≥ 2020
filtered_leases <- leases %>%
  filter(leasedSF >= 10000, year >= 2020)

# Step 3: Convert unemployment to quarterly data
unemployment_q <- unemployment %>%
  mutate(
    quarter = case_when(
      month %in% 1:3   ~ "Q1",
      month %in% 4:6   ~ "Q2",
      month %in% 7:9   ~ "Q3",
      month %in% 10:12 ~ "Q4"
    )
  ) %>%
  group_by(year, quarter, state) %>%
  summarise(
    unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Merge unemployment into leases before aggregation
leases_with_unemp <- filtered_leases %>%
  left_join(unemployment_q, by = c("year", "quarter", "state"))

# Step 5: Aggregate leases to market-quarter level
agg_leases <- leases_with_unemp %>%
  group_by(year, quarter, market) %>%
  summarise(
    total_leased_sf     = sum(leasedSF, na.rm = TRUE),
    lease_count         = n(),
    unemployment_rate   = mean(unemployment_rate, na.rm = TRUE),
    .groups             = "drop"
  )

# Step 6: Aggregate and merge rent data
rent_agg <- rent %>%
  group_by(year, quarter, market) %>%
  summarise(
    overall_rent            = mean(overall_rent, na.rm = TRUE),
    availability_proportion = mean(availability_proportion, na.rm = TRUE),
    .groups                 = "drop"
  )

agg_leases <- agg_leases %>%
  left_join(rent_agg, by = c("year", "quarter", "market"))

# Step 7: Merge occupancy data
agg_leases <- agg_leases %>%
  left_join(
    occupancy %>% select(year, quarter, market, avg_occupancy_proportion),
    by = c("year", "quarter", "market")
  )

# Step 8: Impute missing values using MICE
impute_vars <- agg_leases %>%
  select(overall_rent, availability_proportion, avg_occupancy_proportion, unemployment_rate)

imputed <- mice(impute_vars, m = 5, method = "pmm", seed = 123)
completed <- complete(imputed, 1)

# Step 9: Final feature set with log-transformed variables
features <- agg_leases %>%
  select(year, quarter, market, total_leased_sf, lease_count) %>%
  bind_cols(completed) %>%
  mutate(
    log_total_leased_sf = log1p(total_leased_sf),
    log_overall_rent    = log1p(overall_rent)
  )

# Step 10: Export features
write_csv(features, "data/processed/features.csv")

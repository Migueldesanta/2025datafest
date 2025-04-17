# Load Libraries
library(dplyr)
library(readr)
library(xgboost)
library(caret)
library(Metrics)

# Step 1: Load Data
features <- read_csv("C:/Users/16957/Downloads/Dataset 2025/Dataset 2025/feature/features.csv") %>%
  filter(year > 2021 | (year == 2021 & quarter == "Q4")) %>%
  mutate(
    quarter_num = case_when(
      quarter == "Q1" ~ 1,
      quarter == "Q2" ~ 2,
      quarter == "Q3" ~ 3,
      quarter == "Q4" ~ 4
    ),
    time = year + (quarter_num - 1) / 4,
    time_trend = time,
    rent_x_avail = log1p(overall_rent) * availability_proportion,
    rent_x_unemp = log1p(overall_rent) * unemployment_rate,
    market = as.factor(market)
  )

# Step 2: Lag Features
features_lagged <- features %>%
  arrange(market, time) %>%
  group_by(market) %>%
  mutate(
    lag_log_sf = lag(log1p(total_leased_sf)),
    lag_log_rent = lag(log1p(overall_rent)),
    lag_avail = lag(availability_proportion),
    lag_occ = lag(avg_occupancy_proportion),
    lag_unemp = lag(unemployment_rate),
    lag_rent_x_avail = lag(rent_x_avail),
    lag_rent_x_unemp = lag(rent_x_unemp),
    lead_log_sf = lead(log1p(total_leased_sf))
  ) %>%
  ungroup() %>%
  drop_na()

# Step 3: Prepare Training and Validation Sets
set.seed(42)
split_index <- createDataPartition(features_lagged$lead_log_sf, p = 0.8, list = FALSE)
train_set <- features_lagged[split_index, ]
valid_set <- features_lagged[-split_index, ]

# Step 4: Convert to XGBoost Matrix
X_train <- model.matrix(lead_log_sf ~ . -year -quarter -time -total_leased_sf -lease_count, data = train_set)[, -1]
y_train <- train_set$lead_log_sf
X_valid <- model.matrix(lead_log_sf ~ . -year -quarter -time -total_leased_sf -lease_count, data = valid_set)[, -1]
y_valid <- valid_set$lead_log_sf

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dvalid <- xgb.DMatrix(data = X_valid, label = y_valid)

# Step 5: Train XGBoost Model
xgb_model <- xgboost(
  data = dtrain,
  nrounds = 100,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  verbose = 0
)

# Step 6: Evaluate
valid_pred <- predict(xgb_model, X_valid)
rmse_val <- rmse(y_valid, valid_pred)
mae_val <- mae(y_valid, valid_pred)
r2_val <- R2(valid_pred, y_valid)

cat("Validation RMSE:", rmse_val, "\n")
cat("Validation MAE:", mae_val, "\n")
cat("Validation R^2:", r2_val, "\n")

# Step 7: Forecast 2025 Q1
predict_data <- features %>%
  filter(year == 2024, quarter == "Q4") %>%
  mutate(
    year = 2025,
    quarter = "Q1",
    quarter_num = 1,
    time = 2025.0,
    time_trend = 2025.0,
    lag_log_sf = log1p(total_leased_sf),
    lag_log_rent = log1p(overall_rent),
    lag_avail = availability_proportion,
    lag_occ = avg_occupancy_proportion,
    lag_unemp = unemployment_rate,
    lag_rent_x_avail = log1p(overall_rent) * availability_proportion,
    lag_rent_x_unemp = log1p(overall_rent) * unemployment_rate
  )

X_pred <- model.matrix(~ lag_log_sf + lag_log_rent + lag_avail + lag_occ + lag_unemp +
                         lag_rent_x_avail + lag_rent_x_unemp + time_trend + market, data = predict_data)[, -1]

predict_data$predicted_log_sf <- predict(xgb_model, X_pred)
predict_data$predicted_leased_sf <- expm1(predict_data$predicted_log_sf)
predict_data$prev_leased_sf <- expm1(predict_data$lag_log_sf)
predict_data$predicted_growth_rate <- (predict_data$predicted_leased_sf - predict_data$prev_leased_sf) / predict_data$prev_leased_sf

forecast <- predict_data %>%
  select(market, year, quarter, predicted_leased_sf, predicted_growth_rate) %>%
  mutate(across(where(is.numeric), round, 2))

# Step 8: Save Outputs
write_csv(forecast, "forecast_2025Q1_xgb.csv")
saveRDS(xgb_model, "xgb_model_trained_final.rds")
write_csv(
  tibble(
    RMSE_log = rmse_val,
    MAE_log = mae_val,
    R2 = r2_val
  ),
  "xgb_model_metrics.csv"
)

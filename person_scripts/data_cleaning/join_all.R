library(dplyr)

# --- Join person_vehicle_joined with accident_env ---
merged_data <- person_vehicle_joined %>%
  left_join(accident_env, by = "ACCIDENT_NO") %>%
  mutate(
    # --- Simplify AGE_GROUP into broader life-stage bins ---
    AGE_GROUP_SIMPL = case_when(
      AGE_GROUP %in% c("0-4", "5-12", "13-15", "16-17") ~ "Child_Teen",
      AGE_GROUP %in% c("18-21", "22-25", "26-29") ~ "Young_Adult",
      AGE_GROUP %in% c("30-39", "40-49", "50-59") ~ "Adult",
      AGE_GROUP %in% c("60-64", "65-69", "70+") ~ "Older_Adult",
      AGE_GROUP == "Unknown" ~ "Unknown",
      TRUE ~ "Unknown"
    ),

    # --- Binary flag for driver seating ---
    IS_DRIVER = ifelse(SEATING_POSITION == "D", 1, 0),

    # --- Binary/categorical flag for safety equipment ---
    SAFETY_USED_FLAG = case_when(
      HELMET_BELT_WORN %in% c(1, 3, 6) ~ "Used",            # Seatbelt/restraint/helmet worn
      HELMET_BELT_WORN %in% c(2, 4, 5, 7) ~ "Not_Used",     # Not worn / not fitted
      TRUE ~ "Not_Applicable"                               # Unknown / not appropriate
    ),
    SAFETY_USED_FLAG = factor(
      SAFETY_USED_FLAG,
      levels = c("Used", "Not_Used", "Not_Applicable")
    ),

    # --- Vehicle age (in years) ---
    VEHICLE_AGE_AVG = YEAR - VEHICLE_YEAR_MANUF_AVG
  ) %>%
  # Clean impossible values (negative or unrealistically large ages)
  mutate(
    VEHICLE_AGE_AVG = ifelse(VEHICLE_AGE_AVG < 0 | VEHICLE_AGE_AVG > 60, NA, VEHICLE_AGE_AVG)
  )

# --- Median imputation for missing vehicle age ---
median_vehicle_age <- median(merged_data$VEHICLE_AGE_AVG, na.rm = TRUE)
merged_data <- merged_data %>%
  mutate(
    VEHICLE_AGE_AVG = ifelse(is.na(VEHICLE_AGE_AVG), median_vehicle_age, VEHICLE_AGE_AVG)
  )

# --- Imputation for missing weight data ---
merged_data <- merged_data %>%
  mutate(
    TARE_WEIGHT_AVG = ifelse(
      TARE_WEIGHT_AVG == 0 | is.na(TARE_WEIGHT_AVG),
      predict(lm_tare, newdata = .),
      TARE_WEIGHT_AVG),
  )%>%
  # Drop original field
  select(-VEHICLE_YEAR_MANUF_AVG)

# --- Diagnostics ---
cat("Joined person+vehicle with accident_env\n")
cat("Rows:", nrow(merged_data), "\n")
cat("Columns:", ncol(merged_data), "\n")

# Missingness summary
na_percent <- round(100 * colMeans(is.na(merged_data)), 2)
na_percent <- sort(na_percent[na_percent > 0], decreasing = TRUE)

cat("\n Missing value percentages (top 20):\n")
print(head(na_percent, 20))

cat("\n Median vehicle age used for imputation:", median_vehicle_age, "\n")



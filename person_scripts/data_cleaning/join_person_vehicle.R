library(dplyr)
library(fastDummies)

person_clean <- person %>%
  # Drop irrelevant / high-missing columns
  select(-c(TAKEN_HOSPITAL, LICENCE_STATE, EJECTED_CODE)) %>%

  # Remove rows with missing SEATING_POSITION or VEHICLE_ID
  filter(!is.na(SEATING_POSITION) & !is.na(VEHICLE_ID) & !is.na(SEX))

person_clean <- person_clean %>%
  mutate(
    # Binary target
    INJ_SEVERE_BINARY = ifelse(INJ_LEVEL %in% c(1, 2), 1, 0),

    # Factor for user type
    ROAD_USER_TYPE_DESC = as.factor(ROAD_USER_TYPE_DESC)
  )

person_clean <- person_clean %>%
  dummy_cols(select_columns = "ROAD_USER_TYPE_DESC", remove_selected_columns = TRUE)




person_clean <- person_clean %>%
  mutate(
    ROAD_USER_TYPE_DESC_Other =
      (.[["ROAD_USER_TYPE_DESC_E-scooter Rider"]] == 1 |
         .[["ROAD_USER_TYPE_DESC_Pedestrians"]] == 1 |
         .[["ROAD_USER_TYPE_DESC_Pillion Passengers"]] == 1 |
         .[["ROAD_USER_TYPE_DESC_Not Known"]] == 1) * 1
  ) %>%

  # Drop the original rare categories
  select(
    -c(
      "ROAD_USER_TYPE_DESC_E-scooter Rider",
      "ROAD_USER_TYPE_DESC_Pedestrians",
      "ROAD_USER_TYPE_DESC_Pillion Passengers",
      "ROAD_USER_TYPE_DESC_Not Known"
    )
  ) %>%

  select(
    ACCIDENT_NO, VEHICLE_ID,
    SEX, AGE_GROUP, SEATING_POSITION,
    HELMET_BELT_WORN,
    starts_with("ROAD_USER_TYPE_DESC_"),
    INJ_SEVERE_BINARY
  )


# Print summary of what changed
cat("Rows before cleaning:", nrow(person), "\n")
cat("Rows after cleaning:", nrow(person_clean), "\n")
cat("Columns after cleaning:", ncol(person_clean), "\n")


# --- Stage 1: Join person and vehicle info ---
person_vehicle_joined <- person_clean %>%
  left_join(
    vehicle_agg,
    by = c("ACCIDENT_NO", "VEHICLE_ID")
  )

# --- Diagnostics ---
cat("Person-Vehicle joined dataset created!\n")
cat("Rows:", nrow(person_vehicle_joined), "\n")
cat("Columns:", ncol(person_vehicle_joined), "\n")


# --- Drop the negligible 34 rows ---
person_vehicle_joined <- person_vehicle_joined %>%
  filter(!is.na(FUEL_COMBUSTION_PCT),  !is.na(HELMET_BELT_WORN))

# --- Median imputation for numeric fields ---
person_vehicle_joined <- person_vehicle_joined %>%
  mutate(
    NO_OF_CYLINDERS_AVG = ifelse(is.na(NO_OF_CYLINDERS_AVG) | NO_OF_CYLINDERS_AVG > 20,
                                 median(NO_OF_CYLINDERS_AVG, na.rm = TRUE),
                                 NO_OF_CYLINDERS_AVG),
    VEHICLE_YEAR_MANUF_AVG = ifelse(is.na(VEHICLE_YEAR_MANUF_AVG),
                                    median(VEHICLE_YEAR_MANUF_AVG, na.rm = TRUE),
                                    VEHICLE_YEAR_MANUF_AVG),
    TOTAL_OCCUPANTS_AVG = ifelse(is.na(TOTAL_OCCUPANTS_AVG),
                                 median(TOTAL_OCCUPANTS_AVG, na.rm = TRUE),
                                 TOTAL_OCCUPANTS_AVG)
  )

# Check missingness after join
join_na_summary <- sapply(person_vehicle_joined, function(x) sum(is.na(x)))
join_na_summary <- sort(join_na_summary[join_na_summary > 0], decreasing = TRUE)
print(head(join_na_summary, 15))




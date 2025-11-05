library(purrr)

# Select numeric columns only (excluding ACCIDENT_NO, IDs)
num_cols <- person_vehicle_joined %>%
  select(where(is.numeric))

# Compute correlation + p-value
cor_results <- map_dfr(names(num_cols), function(col) {
  x <- person_vehicle_joined[[col]]
  y <- person_vehicle_joined$TARE_WEIGHT_AVG
  
  # Only use complete pairs
  valid_idx <- complete.cases(x, y)
  
  if (sum(valid_idx) > 50) {  # require enough samples
    test <- cor.test(x[valid_idx], y[valid_idx], method = "pearson")
    tibble(
      Variable = col,
      Correlation = test$estimate,
      P_value = test$p.value,
      N = sum(valid_idx)
    )
  } else {
    tibble(Variable = col, Correlation = NA, P_value = NA, N = sum(valid_idx))
  }
})

# Sort by absolute correlation strength
cor_results <- cor_results %>%
  arrange(desc(abs(Correlation)))

print(head(cor_results, 20))


lm_tare <- lm(
  TARE_WEIGHT_AVG ~ NO_OF_CYLINDERS_AVG +
    TOTAL_OCCUPANTS_AVG +
    VEHICLE_YEAR_MANUF_AVG +
    FUEL_COMBUSTION_PCT +
    FUEL_UNKNOWN_PCT +
    ROAD_USER_TYPE_DESC_Motorcyclists +
    ROAD_USER_TYPE_DESC_Bicyclists +
    ROAD_USER_TYPE_DESC_Drivers +
    ROAD_USER_TYPE_DESC_Passengers,
  data = person_vehicle_joined %>% filter(TARE_WEIGHT_AVG > 0)
)

summary(lm_tare)

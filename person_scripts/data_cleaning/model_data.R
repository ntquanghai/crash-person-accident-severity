library(dplyr)
library(fastDummies)

model_data <- merged_data %>%
  #  Remove unused / redundant columns 
  select(
    -c(
      AGE_GROUP,           # replaced by AGE_GROUP_SIMPL
      SEATING_POSITION,    # replaced by IS_DRIVER
      HELMET_BELT_WORN,    # replaced by SAFETY_USED_FLAG
      ACCIDENT_NO, VEHICLE_ID,  # IDs not useful for modeling
    )
  ) 

#  One-hot encode categorical variables 
cat_vars <- c(
  "SEX",
  "AGE_GROUP_SIMPL",
  "SAFETY_USED_FLAG",
  "TIME_OF_DAY",
  "SPEED_CAT",
  "ROAD_TYPE_GRP",
  "LIGHT_CAT",
  "DCA_GROUP",
  "ACCIDENT_TYPE_GROUP"
)

model_data <- dummy_cols(
  model_data,
  select_columns = cat_vars,
  remove_selected_columns = TRUE,
  ignore_na = TRUE
)

#  Confirm all features are numeric and ready 
model_data <- model_data %>%
  mutate(across(where(is.logical), as.numeric))

cat("Model dataset ready!\n")
cat("Rows:", nrow(model_data), " | Columns:", ncol(model_data), "\n")

# --- Quick sanity check ---
cat("Target variable balance:\n")
print(table(model_data$INJ_SEVERE_BINARY))
print(round(prop.table(table(model_data$INJ_SEVERE_BINARY)) * 100, 2))

library(dplyr)
library(lubridate)

accident_agg <- accident %>%
  mutate(
    # --- Temporal features ---
    YEAR = year(ACCIDENT_DATE),
    MONTH = month(ACCIDENT_DATE),
    IS_WEEKEND = DAY_WEEK_DESC %in% c("Saturday", "Sunday"),
    
    HOUR = as.numeric(substr(ACCIDENT_TIME, 1, 2)),
    TIME_OF_DAY = case_when(
      HOUR >= 5 & HOUR < 12 ~ "Morning",
      HOUR >= 12 & HOUR < 17 ~ "Afternoon",
      HOUR >= 17 & HOUR < 21 ~ "Evening",
      TRUE ~ "Night"
    ),
    
    # --- Speed & road context ---
    SPEED_ZONE_NUM = as.numeric(SPEED_ZONE),
    SPEED_CAT = case_when(
      SPEED_ZONE_NUM >= 30 & SPEED_ZONE_NUM <= 60 ~ "Low",
      SPEED_ZONE_NUM >= 61 & SPEED_ZONE_NUM <= 90 ~ "Medium",
      SPEED_ZONE_NUM >= 91 & SPEED_ZONE_NUM <= 200 ~ "High",
      SPEED_ZONE_NUM > 200 ~ "Unknown",
      TRUE ~ "Unknown"
    ),
    
    
    ROAD_TYPE_GRP = case_when(
      grepl("intersection", ROAD_GEOMETRY_DESC, ignore.case = TRUE) ~ "Intersection",
      grepl("not at intersection|dead end", ROAD_GEOMETRY_DESC, ignore.case = TRUE) ~ "NonIntersection",
      TRUE ~ "Other"
    ),
    
    # --- Lighting ---
    LIGHT_CAT = case_when(
      LIGHT_CONDITION == 1 ~ "Daylight",
      LIGHT_CONDITION == 2 ~ "Dusk_Dawn",
      LIGHT_CONDITION %in% c(3,4,5,6) ~ "Dark",
      TRUE ~ "Unknown"
    ),
    
    # --- DCA grouping (pre-impact pattern) ---
    DCA_GROUP = case_when(
      grepl("pedestrian", DCA_DESC, ignore.case = TRUE) ~ "Pedestrian",
      grepl("intersection|cross|turn", DCA_DESC, ignore.case = TRUE) ~ "Intersection",
      grepl("rear|sideswipe|lane", DCA_DESC, ignore.case = TRUE) ~ "SameDirection",
      grepl("overtaking|merging", DCA_DESC, ignore.case = TRUE) ~ "Overtaking",
      grepl("off carriageway|bend|run off", DCA_DESC, ignore.case = TRUE) ~ "OffCarriageway",
      grepl("parking|reversing", DCA_DESC, ignore.case = TRUE) ~ "ParkingOrReversing",
      TRUE ~ "MiscOther"
    ),
    
    # --- Accident type grouping ---
    ACCIDENT_TYPE_GROUP = case_when(
      grepl("vehicle", ACCIDENT_TYPE_DESC, ignore.case = TRUE) ~ "VehicleCollision",
      grepl("pedestrian|animal", ACCIDENT_TYPE_DESC, ignore.case = TRUE) ~ "PedestrianOrAnimal",
      grepl("object|fixed", ACCIDENT_TYPE_DESC, ignore.case = TRUE) ~ "FixedObject",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(ACCIDENT_NO) %>%
  summarise(
    YEAR = first(YEAR),
    IS_WEEKEND = as.integer(any(IS_WEEKEND, na.rm = TRUE)),
    SPEED_ZONE_NUM = mean(SPEED_ZONE_NUM, na.rm = TRUE),
    MONTH = first(MONTH),
    TIME_OF_DAY = first(TIME_OF_DAY),
    SPEED_CAT = first(SPEED_CAT),
    ROAD_TYPE_GRP = first(ROAD_TYPE_GRP),
    LIGHT_CAT = first(LIGHT_CAT),
    DCA_GROUP = first(DCA_GROUP),
    ACCIDENT_TYPE_GROUP = first(ACCIDENT_TYPE_GROUP),
    SEVERITY = first(SEVERITY),
    .groups = "drop"
  )

cat("acident_agg ready (compact, no leakage). Rows:", nrow(accident_agg),
    " Unique:", length(unique(accident_agg$ACCIDENT_NO)), "\n")

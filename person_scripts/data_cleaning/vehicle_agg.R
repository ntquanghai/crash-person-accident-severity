library(dplyr)

vehicle_agg <- vehicle %>%
  mutate(
    # --- Fuel grouping ---
    FUEL_GROUP = case_when(
      FUEL_TYPE %in% c("P","D","G","M","R") ~ "Combustion",
      FUEL_TYPE == "E" ~ "Electric",
      TRUE ~ "Unknown"
    ),

    # --- Direction grouping ---
    DIR_INIT_N = INITIAL_DIRECTION %in% c("N","NE","NW"),
    DIR_INIT_E = INITIAL_DIRECTION %in% c("E","NE","SE"),
    DIR_INIT_S = INITIAL_DIRECTION %in% c("S","SE","SW"),
    DIR_INIT_W = INITIAL_DIRECTION %in% c("W","NW","SW"),
    DIR_FINAL_N = FINAL_DIRECTION %in% c("N","NE","NW"),
    DIR_FINAL_E = FINAL_DIRECTION %in% c("E","NE","SE"),
    DIR_FINAL_S = FINAL_DIRECTION %in% c("S","SE","SW"),
    DIR_FINAL_W = FINAL_DIRECTION %in% c("W","NW","SW"),

    # --- Compact driver intent grouping ---
    DRIVER_INTENT_GRP = case_when(
      DRIVER_INTENT %in% c("01","02","03") ~ "Straight_Turn",
      DRIVER_INTENT %in% c("09","10","11","12") ~ "Parking_Reversing",
      DRIVER_INTENT %in% c("13","14","15") ~ "Stationary",
      DRIVER_INTENT %in% c("05","06","07","08","16","17","18","19") ~ "Maneuvering",
      TRUE ~ "Unknown"
    ),

    # --- Compact vehicle movement grouping ---
    VEHICLE_MOVEMENT_GRP = case_when(
      VEHICLE_MOVEMENT %in% c("01","02","03") ~ "Straight_Turn",
      VEHICLE_MOVEMENT %in% c("09","10","11","12") ~ "Parking_Reversing",
      VEHICLE_MOVEMENT %in% c("13","14","15") ~ "Stationary",
      VEHICLE_MOVEMENT %in% c("05","06","07","08","16","17","18","19") ~ "Maneuvering",
      TRUE ~ "Unknown"
    ),

    # --- Compact traffic control grouping ---
    TRAFFIC_CONTROL_GRP = case_when(
      TRAFFIC_CONTROL_DESC %in% c("Stop-go lights", "Ped. lights", "Flashing lights") ~ "Signalized",
      TRAFFIC_CONTROL_DESC %in% c("Stop sign", "Giveway sign") ~ "SignBased",
      TRAFFIC_CONTROL_DESC %in% c("Roundabout") ~ "Roundabout",
      TRAFFIC_CONTROL_DESC %in% c("Ped. crossing", "School Flags", "School No flags") ~ "PedestrianControl",
      TRAFFIC_CONTROL_DESC %in% c("RX Gates/Booms", "Police", "Other") ~ "RailOrPolice",
      TRUE ~ "NoneOrUnknown"
    ),

    # --- Binary flags ---
    LAMPS_OFF_FLAG = LAMPS == 2
  ) %>%
  group_by(ACCIDENT_NO, VEHICLE_ID) %>%
  # Since each row has a unique pair of (ACCIDENT_NO, VEHICLE_ID), the new columns do not represent averages (_AVG)
  # or percentages (_PCT). Instead, they represent that particular vehicle's information. These data are not aggregated.,
  # The final report will change these info.
  summarise(
    VEHICLE_YEAR_MANUF_AVG = mean(VEHICLE_YEAR_MANUF, na.rm = TRUE),
    NO_OF_CYLINDERS_AVG = mean(NO_OF_CYLINDERS, na.rm = TRUE),
    TARE_WEIGHT_AVG = mean(TARE_WEIGHT, na.rm = TRUE),
    TOTAL_OCCUPANTS_AVG = mean(TOTAL_NO_OCCUPANTS, na.rm = TRUE),

    FUEL_COMBUSTION_PCT = mean(FUEL_GROUP == "Combustion", na.rm = TRUE),
    FUEL_ELECTRIC_PCT   = mean(FUEL_GROUP == "Electric", na.rm = TRUE),
    FUEL_UNKNOWN_PCT    = mean(FUEL_GROUP == "Unknown", na.rm = TRUE),

    DRIVER_ACTIVE_PCT   = mean(DRIVER_INTENT_GRP %in% c("Straight_Turn", "Maneuvering"), na.rm = TRUE),
    DRIVER_PASSIVE_PCT  = mean(DRIVER_INTENT_GRP %in% c("Parking_Reversing", "Stationary"), na.rm = TRUE),
    
    MOVE_ACTIVE_PCT     = mean(VEHICLE_MOVEMENT_GRP %in% c("Straight_Turn", "Maneuvering"), na.rm = TRUE),
    MOVE_PASSIVE_PCT    = mean(VEHICLE_MOVEMENT_GRP %in% c("Parking_Reversing", "Stationary"), na.rm = TRUE),

    # --- Traffic control proportions ---
    TRAFFIC_SIGNALIZED_PCT   = mean(TRAFFIC_CONTROL_GRP == "Signalized", na.rm = TRUE),
    TRAFFIC_SIGNBASED_PCT    = mean(TRAFFIC_CONTROL_GRP == "SignBased", na.rm = TRUE),
    TRAFFIC_ROUNDABOUT_PCT   = mean(TRAFFIC_CONTROL_GRP == "Roundabout", na.rm = TRUE),
    TRAFFIC_PEDESTRIAN_PCT   = mean(TRAFFIC_CONTROL_GRP == "PedestrianControl", na.rm = TRUE),
    TRAFFIC_RAILPOLICE_PCT   = mean(TRAFFIC_CONTROL_GRP == "RailOrPolice", na.rm = TRUE),
    TRAFFIC_NONEUNKNOWN_PCT  = mean(TRAFFIC_CONTROL_GRP == "NoneOrUnknown", na.rm = TRUE),

    ANY_LAMPS_OFF = as.integer(any(LAMPS_OFF_FLAG, na.rm = TRUE))
  ) %>%
  ungroup()

cat("vehicle_agg ready with traffic control groups — Rows:", nrow(vehicle_agg),
    " Unique accidents:", length(unique(vehicle_agg$ACCIDENT_NO)), "\n")




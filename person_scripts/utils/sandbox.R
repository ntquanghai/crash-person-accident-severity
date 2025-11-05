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
    LAMPS_OFF_FLAG = LAMPS == 2,
    
    ANY_LAMPS_OFF = as.integer(any(LAMPS_OFF_FLAG, na.rm = TRUE))
  ) %>%
  select(
    ACCIDENT_NO, VEHICLE_ID,
    VEHICLE_YEAR_MANUF, NO_OF_CYLINDERS, TARE_WEIGHT, TOTAL_NO_OCCUPANTS,
    FUEL_TYPE, DRIVER_INTENT, VEHICLE_MOVEMENT, TRAFFIC_CONTROL_DESC, LAMPS
  )

cat("vehicle_agg ready with traffic control groups — Rows:", nrow(vehicle_agg),
    " Unique accidents:", length(unique(vehicle_agg$ACCIDENT_NO)), "\n")




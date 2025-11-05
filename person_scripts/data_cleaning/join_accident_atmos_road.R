accident_env <- accident_agg %>%
  left_join(road_agg,  by = "ACCIDENT_NO") %>%
  left_join(atmos_agg, by = "ACCIDENT_NO")


accident_env <- accident_env %>% select(-SPEED_ZONE_NUM, -SEVERITY)


cat("accident_env rows:", nrow(accident_env),
    " unique:", length(unique(accident_env$ACCIDENT_NO)), "\n")


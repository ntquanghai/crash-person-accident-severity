library(tidyverse)
library(data.table)

base_path <- "./"
atmos <- read_csv(file.path(base_path, "atmospheric_cond.csv"))

atmos_agg <- atmos %>%
  mutate(
    ATMOSPH_COND = case_when(
      ATMOSPH_COND == 1 ~ "Clear",
      ATMOSPH_COND == 2 ~ "Raining",
      ATMOSPH_COND == 3 ~ "Snowing",
      ATMOSPH_COND == 4 ~ "Fog",
      ATMOSPH_COND == 5 ~ "Smoke",
      ATMOSPH_COND == 6 ~ "Dust",
      ATMOSPH_COND == 7 ~ "StrongWinds",
      ATMOSPH_COND == 9 ~ "Unknown",
      TRUE ~ "Unknown"
    )
  )

atmos_agg <- atmos_agg %>%
  mutate(
    ATMOSPH_Clear       = as.integer(ATMOSPH_COND == "Clear"),
    ATMOSPH_Raining     = as.integer(ATMOSPH_COND == "Raining"),
    ATMOSPH_Snowing     = as.integer(ATMOSPH_COND == "Snowing"),
    ATMOSPH_Fog         = as.integer(ATMOSPH_COND == "Fog"),
    ATMOSPH_Smoke       = as.integer(ATMOSPH_COND == "Smoke"),
    ATMOSPH_Dust        = as.integer(ATMOSPH_COND == "Dust"),
    ATMOSPH_StrongWinds = as.integer(ATMOSPH_COND == "StrongWinds"),
    ATMOSPH_Unknown     = as.integer(ATMOSPH_COND == "Unknown")
  ) %>% select(-c(ATMOSPH_COND, ATMOSPH_COND_SEQ, ATMOSPH_COND_DESC))  # keep only relevant columns

# atmos_agg <- atmos_agg %>%
#   group_by(ACCIDENT_NO) %>%
#   summarise(across(starts_with("ATMOSPH_"), max, na.rm = TRUE), .groups = "drop")

# Convert to data.table just once
atmos_dt <- as.data.table(atmos_agg)

# Collapse duplicates with max() across one-hot columns
system.time({
  atmos_agg <- atmos_dt[
    , lapply(.SD, max, na.rm = TRUE),
    by = ACCIDENT_NO,
    .SDcols = patterns("^ATMOSPH_")
  ]
})

# Back to tibble if you prefer tidyverse downstream
atmos_agg <- as_tibble(atmos_agg)


cat("Unique ACCIDENT_NO in atmos_agg:", length(unique(atmos_agg$ACCIDENT_NO)), "\n")
cat("Total rows in atmos_agg:", nrow(atmos_agg), "\n")


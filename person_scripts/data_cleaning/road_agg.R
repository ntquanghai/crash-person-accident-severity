library(tidyverse)
library(data.table)


base_path <- "./"
road <- read_csv(file.path(base_path, "road_surface_cond.csv"))

road_agg <- road %>%
  mutate(
    SURFACE_COND = case_when(
      SURFACE_COND == 1 ~ "Dry",
      SURFACE_COND == 2 ~ "Wet",
      SURFACE_COND == 3 ~ "Muddy",
      SURFACE_COND == 4 ~ "Snowy",
      SURFACE_COND == 5 ~ "Icy",
      SURFACE_COND == 9 ~ "Unknown",
      TRUE ~ "Unknown"
    )
  )

road_agg <- road_agg %>%
  mutate(
    SURFACE_Dry     = as.integer(SURFACE_COND == "Dry"),
    SURFACE_Wet     = as.integer(SURFACE_COND == "Wet"),
    SURFACE_Muddy   = as.integer(SURFACE_COND == "Muddy"),
    SURFACE_Snowy   = as.integer(SURFACE_COND == "Snowy"),
    SURFACE_Icy     = as.integer(SURFACE_COND == "Icy"),
    SURFACE_Unknown = as.integer(SURFACE_COND == "Unknown")
  ) %>%
  # Drop raw fields and keep encoded ones
  select(-c(SURFACE_COND, SURFACE_COND_SEQ, SURFACE_COND_DESC)) %>%
  select(ACCIDENT_NO, starts_with("SURFACE_"))

# Convert to data.table just once
road_dt <- as.data.table(road_agg)

# Collapse duplicates with max() across one-hot columns
system.time({
  road_agg <- road_dt[
    , lapply(.SD, max, na.rm = TRUE),
    by = ACCIDENT_NO,
    .SDcols = patterns("^SURFACE_")
  ]
})

# Back to tibble if you prefer tidyverse downstream
road_agg <- as_tibble(road_agg)


print(head(road_agg))

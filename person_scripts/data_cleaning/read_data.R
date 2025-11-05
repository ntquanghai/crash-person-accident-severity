library(tidyverse)
library(digest)

base_path <- "./"

# Load csv files
accident <- read_csv(file.path(base_path, "accident.csv")) 
atmos    <- read_csv(file.path(base_path, "atmospheric_cond.csv"))
person   <- read_csv(file.path(base_path, "person.csv"))
road     <- read_csv(file.path(base_path, "road_surface_cond.csv"))
vehicle  <- read_csv(file.path(base_path, "vehicle.csv"))

# View columns
cat("\n accident.csv \n"); print(colnames(accident))
cat("\n atmospheric_cond.csv \n"); print(colnames(atmos))
cat("\n person.csv \n"); print(colnames(person))
cat("\n road_surface_cond.csv \n"); print(colnames(road))
cat("\n vehicle.csv \n"); print(colnames(vehicle))


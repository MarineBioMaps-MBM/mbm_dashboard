# Load packages
librarian::shelf(shiny, shinydashboard, shinyWidgets, tidyverse, leaflet, shinycssloaders, fresh, sf, janitor)

# Read in study regions boundaries
sr_boundary.dir <- "/capstone/marinebiomaps/data/MLPA_Study_Regions"
sr_boundaries <- sf::st_read(file.path(sr_boundary.dir, "Marine_Life_Protection_Act_Study_Regions_-_R7_-_CDFW_[ds3178].shp")) |> 
  clean_names() |> 
  sf::st_transform('+proj=latlong +datum=WGS84')

# Read in MPA boundaries
boundary.dir <- "/capstone/marinebiomaps/data/MPA_boundaries"
mpas <- sf::st_read(file.path(boundary.dir, "California_Marine_Protected_Areas_[ds582].shp")) |> 
  clean_names() |>
  sf::st_transform('+proj=latlong +datum=WGS84') |> 
  sf::st_make_valid()



# Data for NCSR study region

# Read in ncsr substrate data
rds.dir <- "/capstone/marinebiomaps/data/rds-files/"
ncsr_substrate <- readRDS(file.path(rds.dir, "ncsr_substrate.rds")) |> 
  sf::st_transform('+proj=latlong +datum=WGS84')

# ncsr mpa data
ncsr_mpas <- mpas |> 
  filter(study_regi == "NCSR")

# NCSR biota data
ncsr_biota <- readRDS(file.path(rds.dir, "ncsr_biota.rds")) |> 
  sf::st_transform('+proj=latlong +datum=WGS84')







# Data for NCCSR study region

# Read in nccsr substrate data
rds.dir <- "/capstone/marinebiomaps/data/rds-files/"
nccsr_substrate <- readRDS(file.path(rds.dir, "nccsr_substrate.rds")) |> 
  sf::st_transform('+proj=latlong +datum=WGS84')

# nccsr mpa data
nccsr_mpas <- mpas |> 
  filter(study_regi == "NCCSR")










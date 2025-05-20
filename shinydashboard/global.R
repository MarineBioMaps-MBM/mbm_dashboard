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

# Read in ncsr substrate data
rds.dir <- "/capstone/marinebiomaps/data/rds-files/"
ncsr_substrate <- readRDS(file.path(rds.dir, "ncsr_substrate.rds")) |> 
  sf::st_transform('+proj=latlong +datum=WGS84')

ncsr_mpas <- mpas |> 
  filter(study_regi == "NCSR")
  
ncsr_biota <- readRDS(file.path(rds.dir, "ncsr_biota.rds")) |> 
  sf::st_transform('+proj=latlong +datum=WGS84')
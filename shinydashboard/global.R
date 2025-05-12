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


  

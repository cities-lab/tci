# This script prepares the workspace and file directories for calculating 
# travel time and travel cost with OHAS data

# Load required packages
  require(ggmap)
  require(scales)

# Set workspace
  setwd("~/tci")
  var_list.0 <- ls()

  project.name <- 'Corvallis'
  method.name <- 'OHAS'
  year <- '2011'

##source common settings, which may be overrided below
  source("code/settings.R")
  # Define unit costs for OHAS
  source("code/OHAS/settings_OHAS.R")

## settings (using default settings in code/settings.R)
#   INPUT_DIR <- 'data/Survey/OHAS'
#   OUTPUT_DIR <- file.path('output/Survey/OHAS/Corvallis', unit.name)
#   dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
#   # # whether to save intermediate results
#   # SAVE.INTERMEDIARIES <- TRUE
#   INTERMEDIATE_DIR <- "output/intermediate/OHAS/Corvallis"
#   dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)

## read OHAS hh (household) and linkedTrip table
  load(file.path(INPUT_DIR, "OHAS_Final.Rdata"))
  ##TODO: needs to filter Corvallis data
  TAZ.shpfile <- file.path(INPUT_DIR, "Corvallis/shp/TAZ.shp")
  TAZ.id_name <- "TAZ"
  
  districts <- readOGR(dsn = file.path(INPUT_DIR, "Corvallis/shp"), layer = "districts")
  districts <- fortify(districts, region="DISTRICT")

##start scripts
  source("code/functions.R")
  source("code/OHAS/prepare_Corvallis2011.R")
  source("code/OHAS/compute.R")
  source("code/OHAS/plot.R")

##clean up
  var_list.1 <- ls()
  rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
  rm(var_list.1)
# This script prepares the workspace and file directories for calculating 
# travel time and travel cost with OHAS data
  require(ggmap)
  require(scales)

# Set workspace
  setwd("~/tci")
  var_list.0 <- ls()

  project.name <- 'Portland'
  method.name <- 'survey'
  year <- '2011'
  
##source common settings, which may be overrided below
  source("code/settings.R")
  # Define unit costs for OHAS
  source("code/survey/settings_OHAS.R")

# ## settings (using default settings in code/settings.R)
  #INPUT_DIR <- 'data/Portland2011'
  #OUTPUT_DIR <- file.path('output/survey/Portland', unit.name)
  #dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
  # # whether to save intermediate results
  #SAVE.INTERMEDIARIES <- TRUE
  #INTERMEDIATE_DIR <- "output/intermediate/survey/Portland"
  #dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)

## read OHAS hh (household) and linkedTrip table
  load(file.path(INPUT_DIR, "OHAS_Final.Rdata"))
  load(file.path(INPUT_DIR, "districts.RData"))
  TAZ.shpfile <- file.path(INPUT_DIR, "shp/TAZ.shp")
  TAZ.id_name <- "newtaz"
  districts <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "districts")
  districts <- fortify(districts, region="DISTRICT")

##start scripts
  source("code/functions.R")
  source("code/survey/prepare_Portland2011.R")
  source("code/survey/compute.R")
  source("code/survey/plot.R")

##clean up
  if (CLEAN.UP) clean.up(var_list.0)

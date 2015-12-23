# This script organizes Portland_94 data
# There are three parts: part1 loads data file; part2 generate linedk trips; part3 calculate trip cost 
  require(ggmap)
  require(tidyr)
  require(scales)
  require(kimisc)
  
# Set workspace
  get_dirs <- function(my.path, WD, rel.dir) {
    if (!is.null(my.path)){
      this.dir <- normalizePath(dirname(my.path))
      WD <- strsplit(this.dir, rel.dir)[[1]]
      setwd(WD)
      cat("Set working directory:", getwd())
    } else {
      # this file is not being sourced or invoked as a R script
      if(!WD) stop("source() this file or manually setwd() to the parent directory of 'code'.")
      this.dir <- file.path(WD, rel.dir)
    }
    parent.dir <- normalizePath(file.path(this.dir, ".."))
    
    return(list(this=this.dir, parent=parent.dir))
  }

# Setting    
  var_list.0 <- ls()
  
  project.name <- 'Portland'
  method.name <- 'survey'
  year <- '1994'

  rel.dir <- file.path('code', method.name) #my path relative to WD
  dirs <- get_dirs(my.path = thisfile(),
                   WD = getwd(),
                   rel.dir = rel.dir)  
  
# data.source <- "Portland94"
#unit.name <- "minutes" # "dollars"

##source common settings, which may be overrided below
  source(file.path(dirs$parent, "settings.R"))
  source(file.path(dirs$parent, "functions.R"))

## define unit cost for Portland 1994 survey data
  # distance-based monetary cost per mile
  # http://www.portlandfacts.com/cost_of_transit_&_cars.html
  # http://portlandtaxi.net/rates.php
  ## time-equivalent monetary cost per mile, which can be specific to income group
  
  # mode
  MODE <- c(1:8) 
  MdNames <- c("other", "walk", "bicycle", "schol bus", "public bus", "MAX", "personal vehicle", "non-personal vehicle") 
  names(MODE) <- MdNames
  
  constant <- rep(0, length(MODE))
  
  # minutes unit cost 
  VOT <- rep(1, length(MODE)) * minutes.per.hour
  mcpm <- c(29.6, 0, 0, 0, 101.0, 138.0, 59.2, 59.2) * minutes.per.cent
  unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit.name = "minutes")
  
  # dollars unit cost 
  VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.5, 0.5) * hourly.wage
  mcpm <- c(29.6, 0, 0, 0, 101.0, 138.0, 59.2, 59.2) / cents.per.dollar
  unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit.name = "dollars")
  
  # Combine unitcosts of different units   
  unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)
  
  unitcosts <- unitcosts.list[[unit.name]]

## path settings
  #INPUT_DIR <- 'data/Portland1994'
  #OUTPUT_DIR <- file.path('output/survey/Portland1994', unit.name)
  #dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
  ## # whether to save intermediate results
  ## SAVE.INTERMEDIARIES <- TRUE
  #INTERMEDIATE_DIR <- "output/intermediate/survey/Portland1994"
  #dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)

## Preload data files
  TAZPoly1994.shpfile <- file.path(INPUT_DIR, "shp/taz1260.shp")
  districtsPoly.shpfile <- file.path(INPUT_DIR, "shp/districts.shp")
  
  districts <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "districts")
  districts <- fortify(districts, region="DISTRICT")

  load(file.path(INPUT_DIR, "portland_94.RData"))

## Define functions to calculate tcost 
  source(file.path(dirs$this, "prepare_Portland1994.R"))
  source(file.path(dirs$this, "compute.R"))
  source(file.path(dirs$this, "plot.R"))

##clean up
  if (CLEAN.UP) clean.up(var_list.0)

# This script prepares the workspace and file directories for calculating 
# travel time and travel cost with OHAS data
require(ggmap)
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


# project specific settings
  var_list.0 <- ls()
  
  project.name <- 'TampaBay'
  method.name <- 'survey'
  year <- '1996'
  
  rel.dir <- file.path('code', method.name) #my path relative to WD
  dirs <- get_dirs(my.path = thisfile(),
                   WD = getwd(),
                   rel.dir = rel.dir)  
  ##source common settings, which may be overrided below
  source(file.path(dirs$parent, "functions.R"))
  source(file.path(dirs$parent, "settings.R"))

# Define unit cost 
  # tripmode 
  # Private vehicle -1
  # Bus - 2
  # Walk/Bike - 3
  # Other - 4
  
  # this configuration converts travel costs to time
  MODE <- c(1:4) 
  MdNames <- c("Private vehicle", "Bus", "Walk/Bike", "Other")
  names(MODE) <- MdNames
  constant <- rep(0, length(MODE))
  
  # Unit cost by minutes
  VOT <- rep(1, length(MODE)) * minutes.per.hour  # hours to minutes
  mcpm <- c(59.2, 101.0, 0, 29.6)* minutes.per.cent
  
  unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit.name = "minutes")
  
  # unit cost by dollars
  # VOT in $/hour
  VOT <- c(0.5, 0.35, 0.5, 0.5)* hourly.wage
  mcpm <- c(59.2, 101.0, 0, 29.6)/ cents.per.dollar
    
  unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit.name = "dollars")
  
  
  # Combine unitcosts of different units   
  unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)
  unitcosts <- unitcosts.list[[unit.name]]
  
##start scripts
  source(file.path(dirs$this, "prepare_TampaBay1996.R"))
  source(file.path(dirs$this, "compute.R"))
  source(file.path(dirs$this, "plot.R"))
  
  ##clean up
  if (CLEAN.UP) clean.up(var_list.0)  
  
  
  
  
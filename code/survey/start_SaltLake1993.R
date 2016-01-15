# This script calculate and plot travel costs in Salt Lake City with 1996 survey data 
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
  
  project.name <- 'SaltLakeCity'
  method.name <- 'survey'
  year <- '1993'
  
  rel.dir <- file.path('code', method.name) #my path relative to WD
  dirs <- get_dirs(my.path = thisfile(),
                   WD = getwd(),
                   rel.dir = rel.dir)  
##source common settings, which may be overrided below
  source(file.path(dirs$parent, "functions.R"))
  source(file.path(dirs$parent, "settings.R"))
  
# Define unit cost 
  # Define unit cost 
  # Mode 
  #  Value    Label
  #     1    walk
  #     2    driver
  #     3    passenger
  #     4    UTA bus
  #     5    other public transit/taxi
  #     6    school bus
  #     7    motorcycle
  #     8    bicycle
  #     9    other
  
  # this configuration converts travel costs to time
  MODE <- c(1:9) 
  MdNames <- c("walk", "driver", "passenger", "UTA bus", "other public transit/taxi", "school bus", "motorcycle", "bicycle", "other")
  names(MODE) <- MdNames
  constant <- rep(0, length(MODE))
  
  # Unit cost by minutes
  VOT <- rep(1, length(MODE)) * minutes.per.hour  # hours to minutes
  mcpm <- c(0, 59.2, 59.2, 101.0, 260.0, 0, 29.6, 0, 29.6)* minutes.per.cent
  
  unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit.name = "minutes")
  
  # unit cost by dollars
  # VOT in $/hour
  VOT <- c(0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.35) * hourly.wage
  
  # mcpm in $/mile
  mcpm <- c(0, 59.2, 59.2, 101.0, 260.0, 0, 29.6, 0, 29.6)/cents.per.dollar
  unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit.name = "dollars")
  
  # Combine unitcosts of different units   
  unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)
  
  unitcosts <- unitcosts.list[[unit.name]]
  

##start scripts
  source(file.path(dirs$this, "prepare_SaltLakeCity1993.R"))
  source(file.path(dirs$this, "compute.R"))
  source(file.path(dirs$this, "plot.R"))
 
 ##clean up
   if (CLEAN.UP) clean.up(var_list.0)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
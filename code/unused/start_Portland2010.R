# This script prepares the workspace and file directories for calculating travel time 
# and travel cost with cluster-based methods 

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

  var_list.0 <- ls()
  
  method.name <- "cluster"
  project.name <- "Portland"
  year <- "2010"
  unit.name <- 'minutes'
  
  rel.dir <- file.path('code', method.name) #my path relative to WD
  dirs <- get_dirs(my.path = thisfile(),
                   WD = getwd(),
                   rel.dir = rel.dir)


## default settings
  source(file.path(dirs$parent, "functions.R"))
  source(file.path(dirs$parent, "settings.R"))
  
  source(file.path(dirs$this, "settings.R")) #unitcosts
  
  #  Inc levels
  #Ic <- c("lowInc", "midInc", "highInc")
  # Trip Purpose
  #Pr <- c("hbw", "hbs", "hbr", "hbo")
  # Define the travel modes
  #Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")
  # Define time period 
  Tp <- c("peak", "offPeak")
  
  unitcosts$mcpm <- 0 #keep track of travel time only for diagnostic purpose
  ## set directories (use defaults)  
  #INPUT_DIR <- 'data/Corvallis2011'
  
  tdist.vary.by.tp <- FALSE
  ttime.in.minutes <- TRUE
  get.PA.from.OD <- FALSE  # get PA matrix from reversing OD matrix
  get.OD.from.PA <- FALSE  # get OD matrix from reversing PA matrix
  
  
# Directories  
  path.tdm <- file.path(INPUT_DIR, "TDM")
  file.trips <- file.path(path.tdm, "PrIcMdTrips.omx")      #output/intermediate/ModeTrips.omx
  file.ttime <- file.path(path.tdm, "travelTimeSkims.omx")  #TDM/Skims.omx
  file.tdist <- file.path(path.tdm, "travelDistanceSkims.omx") #TDM/DistanceSkims.omx
  
##start scripts
  source(file.path(dirs$this, "prepare_Portland2010.R"))
  #this step below takes a long time; skip it if its intermediate results have been saved
  #source(file.path(dirs$this, "identify_centers.R"))
  #source(file.path(dirs$this, "compute_md_prob_trips.R"))
  #source(file.path(dirs$this, "compute_tcost.R"))
  #source(file.path(dirs$this, "aggregate_tcost.R"))
  #source(file.path(dirs$this, "aggregate_by_geo.R"))
  source(file.path(dirs$this, "compute.R"))
  source(file.path(dirs$this, "plot.R"))

##clean up
  if (CLEAN.UP) clean.up(var_list.0)

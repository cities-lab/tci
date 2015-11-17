# Load required packages
  require(ggmap)
  require(scales)
  require(kimisc)

# Define functions
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
  
  run_scenario <- function(scenario=NULL, year=NULL) {
    path.tdm <- file.path(INPUT_DIR, "/TDM")
    file.trips <- file.path(path.tdm, "PrIcTpMdTrips.omx")
    file.ttime <- file.path(path.tdm, "travelTimeSkims.omx")
    file.tdist <- file.path(path.tdm, "travelDistanceSkims.omx")
    
    if (get.PA.from.OD | get.OD.from.PA) 
      period.factors <- read.csv(file.path(path.tdm, 'period_factors.csv'), header=T)
    
    load(file.path(INPUT_DIR, "Zi.RData"))
    load(file.path(INPUT_DIR, "districts.RData"))
    load(file.path(INPUT_DIR, "hhs.ZiIc.RData"))
    
    taz <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "TAZ")
    taz <- fortify(taz, region="TAZ")
    
    districts.geo <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "districts")
    districts.geo <- fortify(districts.geo, region="DISTRICT")
    
    ##start scripts
    source(file.path(dirs$this, "compute.R"))
    source(file.path(dirs$this, "plot.R"))
    
    ##clean up
    if (CLEAN.UP) clean.up(var_list.0)
  }
  
# Settings
  var_list.0 <- ls()
  
  method.name <- "cluster"
  project.name <- "Corvallis"
  #year <- "2010"
  unit.name <- 'minutes'

# Set workspace    
  rel.dir <- file.path('code', method.name) #my path relative to WD
  dirs <- get_dirs(my.path = thisfile(),
                   WD = getwd(),
                   rel.dir = rel.dir)
  
  ## default settings
  source(file.path(dirs$parent, "functions.R"))
  
  #scenario.names <- c('2010', '2030Preferred', '2030Preferred_Scen1')
  scenario.names <- c('2010')
  for (scenario.name in scenario.names) {
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
    
    run_scenario(scenario.name)
  }
  
## set Directories

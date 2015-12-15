# Load required packages
  require(ggmap)
  require(scales)
  require(kimisc)
  
 # stop("refactoring yet to be finished")
  
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

# Settings
  var_list.0 <- ls()
  
  method.name <- "cluster"
  project.name <- "Portland"
  year <- ""
  unit.name <- 'minutes'

# Set workspace    
  rel.dir <- file.path('code', method.name) #my path relative to WD
  dirs <- get_dirs(my.path = thisfile(),
                   WD = getwd(),
                   rel.dir = rel.dir)

  source(file.path(dirs$parent, "functions.R"))

# run through scenarios
  ## default input directory structure: data/project.name/scenario.name[year]/
  ## default ouput directory structure: output/project.name/scenario.name[year]/method.name/unit.name
  
  scenario.names <- c('2010')
  #scenario.names <- c('2040NB', '2040State', '2040TFC')
  
  for (scenario.name in scenario.names) {
    source(file.path(dirs$parent, "settings.R"))
    source(file.path(dirs$this, "settings.R")) #unitcosts
    
    unitcosts$mcpm <- 0 #keep track of travel time only for diagnostic purpose
    
    tdist.vary.by.tp <- TRUE
    ttime.in.minutes <- TRUE
    get.PA.from.OD <- FALSE  # get PA matrix from reversing OD matrix
    get.OD.from.PA <- FALSE  # get OD matrix from reversing PA matrix
    
    path.tdm <- file.path(INPUT_DIR, "TDM")
    file.trips <- file.path(path.tdm, "PrIcMdODTrips.omx")
    file.ttime <- file.path(path.tdm, "travelTimeSkims.omx")
    file.tdist <- file.path(path.tdm, "travelDistanceSkims.omx")
    
    if (get.PA.from.OD | get.OD.from.PA) 
      period.factors <- read.csv(file.path(path.tdm, 'period_factors.csv'), header=T)
    
    load(file.path(INPUT_DIR, "Zi.RData"))
    load(file.path(INPUT_DIR, "hhs.ZiIc.RData"))
    
    taz.id <- 'newtaz'
    district.id <- 'DISTRICT'
    taz <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "TAZ")
    taz <- fortify(taz, region=taz.id)
    
    distr.file <- file.path(INPUT_DIR, "districts.RData")
    if (file.exists(distr.file)) {
      load(distr.file)
      districts.geo <- readOGR(dsn = file.path(INPUT_DIR, "shp"), layer = "districts")
      districts.geo <- fortify(districts.geo, region=district.id)
    }
    
    ##start scripts
    source(file.path(dirs$this, "define_basket.R"))
    source(file.path(dirs$this, "compute.R"))
    source(file.path(dirs$this, "plot.R"))
    
    ##clean up
    if (CLEAN.UP) clean.up(var_list.0)    
  }


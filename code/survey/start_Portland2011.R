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

  project.name <- 'Portland'
  method.name <- 'survey'
  year <- '2011'

  rel.dir <- file.path('code', method.name) #my path relative to WD
  dirs <- get_dirs(my.path = thisfile(),
                   WD = getwd(),
                   rel.dir = rel.dir)  
  
##source common settings, which may be overrided below
  source(file.path(dirs$parent, "functions.R"))
  source(file.path(dirs$parent, "settings.R"))
  # Define unit costs for OHAS
  source(file.path(dirs$this, "settings_OHAS.R"))

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
  #source(file.path(dirs$this, "prepare_Portland2011.R"))
  source(file.path(dirs$this, "compute.R"))
  source(file.path(dirs$this, "plot.R"))

##clean up
  if (CLEAN.UP) clean.up(var_list.0)

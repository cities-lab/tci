# Setting 
# Load required packages
require(dplyr)
require(grid)
require(scales)
require(gridExtra) #require(Rmisc)
require(ggplot2)
require(ggmap)
require(rgdal)

# Define function 
tcost_by_dataSource_unit <- function(data.source=NULL, unit.name=NULL) {
  # Set global objects 
  data.source <<- data.source
  unit.name <<- unit.name
  
  if (data.source=="OHAS/Portland") {
    source("code/OHAS/start.R")
    
  } else if (data.source=="OHAS/Corvallis") {
    source("code/OHAS/start_corvallis.R")
    
  } else if (data.source=="Portland94") {
    source("code/OHAS/start94.R")
    
  } else if (data.source=="WFRC_SaltLake") {
    source("code/misc/WFRC_SaltLake.R")
    
  } else if (data.source=="NHTS09") {
    source("code/misc/NHTS09.R")
    
  }
  else {
    print(paste( "data.source:", data.source, "does not exist.", sep=" "))
    
  }
  
  # remove data.source and unit in global enviroment
  rm(list = c("data.source", "unit.name"), pos = ".GlobalEnv")
}

# Calculate travel cost 
setwd("~/tci")
tcost_by_dataSource_unit(data.source="OHAS/Portland", unit.name="minutes")
tcost_by_dataSource_unit(data.source="OHAS/Portland", unit.name="dollars")

tcost_by_dataSource_unit(data.source="OHAS/Corvallis", unit.name="minutes")
tcost_by_dataSource_unit(data.source="OHAS/Corvallis", unit.name="dollars")

tcost_by_dataSource_unit(data.source="Portland94", unit.name="minutes")
tcost_by_dataSource_unit(data.source="Portland94", unit.name="dollars")

tcost_by_dataSource_unit(data.source="WFRC_SaltLake", unit.name="minutes")
tcost_by_dataSource_unit(data.source="WFRC_SaltLake", unit.name="dollars")

tcost_by_dataSource_unit(data.source="NHTS09", unit.name="minutes")
tcost_by_dataSource_unit(data.source="NHTS09", unit.name="dollars")



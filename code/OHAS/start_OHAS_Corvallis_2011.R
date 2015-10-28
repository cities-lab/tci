# This script prepares the workspace and file directories for calculating 
# travel time and travel cost with OHAS data

# Load required packages
require(ggmap)
require(scales)

# Set workspace
setwd("~/tci")

var_list.0 <- ls()

##source common settings, which may be overrided below
source("code/settings.R")

# Define OHAS unit costs 
MODE <- c(1:10,97, 21, 22, 23, 24, 25) # 1:10 and 97 are coded in OHAS; 1:2 and 21:25 are coded for TDM
MdNames <- c("walk", "bike", "auto / van/ truck driver", "auto / van / truck passenger", "bus", "rail", "dial-a-ride/paratransit", "taxi",  
             "school bus", "carpool / vanpool", "other (specify)", "driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus")
names(MODE) <- MdNames

constant <- rep(0, length(MODE))

# unit cost by minutes 
hourly.wage <- 60 
VOT <- rep(1, length(MODE)) * hourly.wage
mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0)*60 / (100 * 24.77)
unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit="minutes")

# unit cost by dollars 
hourly.wage <- 24.77
VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35) * hourly.wage
mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0) / 100
unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit="dollars")

# Combine unitcosts of different units   
unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)

# data.source <- "OHAS/Corvallis"
# unit.name <- "minutes" # "dollars" 
unitcosts <- OHAS.unitcosts.list[[unit.name]]

# ## settings
# INPUT_DIR <- 'data/'
# OUTPUT_DIR <- 'output/Survey/OHAS/Corvallis/Minutes'
# dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
# # whether to save intermediate results
# SAVE.INTERMEDIARIES <- TRUE
# INTERMEDIATE_DIR <- "output/intermediate/OHAS/Corvallis"
# dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)
# 
# # make names for household income groups, trip purpose and calculation method
# IcNames <- c("Low Income", "Mid Income", "High Income")
# Ic <- c("lowInc", "midInc", "highInc")
# names(IcNames) <- Ic
# 
# PrNames <- c("Work", "Shopping", "Recreation", "Other")
# Pr <- c("hbw", "hbs", "hbr", "hbo")
# names(PrNames) <- Pr
# 
# CmNames <- c("mintcost", "avgtcost", "maxtcost")
# Cm <- c("min", "avg", "max")
# names(CmNames) <- Cm  

## read OHAS hh (household) and linkedTrip table
load(file.path(INPUT_DIR, "OHAS_Final.Rdata"))
TAZ.shpfile <- file.path(INPUT_DIR, "shp/Corvallis/TAZ.shp")
TAZ.id_name <- "TAZ"

districts <- readOGR(dsn = file.path(INPUT_DIR, "shp/Corvallis"), layer = "districts")
districts <- fortify(districts, region="DISTRICT")

##start scripts
source("code/OHAS/functions.R")
source("code/OHAS/prepare_data_corvallis.R")
source("code/OHAS/compute_tcost.R")
source("code/OHAS/plot_tcost.R")

##clean up
var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1)
# This script organizes Portland_94 data
# There are three parts: part1 loads data file; part2 generate linedk trips; part3 calculate trip cost 
require(dplyr)
require(rhdf5)
require(maptools)
require(rgeos)
require(rgdal)

# Settings
# Set workspace
setwd("~/tci")
var_list.0 <- ls()

## settings
INPUT_DIR <- 'data/'
OUTPUT_DIR <- 'output/OHAS/portland_94'
dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
# whether to save intermediate results
SAVE.INTERMEDIARIES <- TRUE
INTERMEDIATE_DIR <- "output/intermediate/OHAS/portland_94"
dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)

# make names for household income groups, trip purpose and calculation method
IcNames <- c("Low Income", "Mid Income", "High Income")
Ic <- c("lowInc", "midInc", "highInc")
names(IcNames) <- Ic

PrNames <- c("Work", "Shopping", "Recreation", "Other")
Pr <- c("hbw", "hbs", "hbr", "hbo")
names(PrNames) <- Pr

CmNames <- c("mintcost", "avgtcost", "maxtcost")
Cm <- c("min", "avg", "max")
names(CmNames) <- Cm


# unit travel costs by mode
# unit travel costs can be differentiated by income 
# by adding an "inc.level" column to the data.frame
# with value "lowInc", "midInc", "highInc"

# this configuration converts travel costs to $
hourly.wage <- 24.77
MODE <- c(1:8) # 1:10 and 97 are coded in OHAS; 1:2 and 21:25 are coded for TDM
MdNames <- c("other", "walk", "bicycle", "schol bus", "public bus", "MAX", "personal vehicle", "non-personal vehicle") 

names(MODE) <- MdNames


VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.5, 0.5) * hourly.wage


# distance-based monetary cost per mile
# http://www.portlandfacts.com/cost_of_transit_&_cars.html
# http://portlandtaxi.net/rates.php
# mcpm <- c(29.6, 0, 0, 101.0, 101.0, 138.0, 0, 59.2, 59.2) / 100
# mcpm <- rep(0, length(MODE))

## time-equivalent monetary cost per mile, which can be specific to income group
mcpm <- c(29.6, 0, 0, 0, 101.0, 138.0, 59.2, 59.2) / (100 * hourly.wage)

unitcosts <- data.frame(MODE, VOT, mcpm)

# load(file.path(INPUT_DIR, "portland_94.RData"))

# Define functions to calculate tcost 
source("code/OHAS/functions.R")
source("code/OHAS/prepare_data94.R")
source("code/OHAS/compute_tcost.R")
source("code/OHAS/plot_tcost.R")

##clean up
var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1)



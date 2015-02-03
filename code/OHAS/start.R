# This script prepares the workspace and file directories for calculating 
# travel time and travel cost with OHAS data

# Set workspace
setwd("~/tci")

var_list.0 <- ls()

INPUT_DIR <- 'data/'
OUTPUT_DIR <- 'output/OHAS'
dir.create(file.path(OUTPUT_DIR), showWarnings = FALSE)
# whether to save intermediate results
SAVE.INTERMEDIARIES <- TRUE
INTERMEDIATE_DIR <- "output/intermediate/OHAS"
dir.create(file.path(INTERMEDIATE_DIR), showWarnings = FALSE)

# mode specific VOT
hourly.wage <- 24.77
mode <- c(1:10,97) #as being coded in OHAS
#1  WALK
#2  BIKE
#3	AUTO / VAN/ TRUCK DRIVER
#4	AUTO / VAN / TRUCK PASSENGER
#5	BUS
#6	RAIL
#7	DIAL-A-RIDE/PARATRANSIT
#8	TAXI
#9	SCHOOL BUS
#10	CARPOOL / VANPOOL
#97	OTHER (SPECIFY)
VOT <- c(0.5,0.5,0.5,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.5) * hourly.wage
VOT.by.mode <- data.frame(mode, VOT)

##start scripts
source("code/OHAS/compute_tcost.R")
source("code/OHAS/plot_tcost.R")
var_list.1 <- ls()
rm(list=var_list.1[!(var_list.1 %in% var_list.0)])
rm(var_list.1)

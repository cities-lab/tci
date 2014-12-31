# This script prepares the workspace and file directories for calculating 
# travel time and travel cost with OHAS data

# Set workspace
setwd("~/tci")

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

# wether to save intermediate results
SAVE.INTERMEDIARIES <- TRUE

##
source("code/OHAS/summarize_tcost.R")
source("code/OHAS/plot_tcost.R")

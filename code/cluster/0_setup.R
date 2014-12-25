# This script prepares the workspace and file directories for calculating travel time 
# and travel cost with employment centres methods 

# Set workspace
setwd("~/tci")

# Define income group abbreviation
Ic <- c("lowInc", "midInc", "highInc")

# Define trip purpose abbreviation
# The purposes for this study (now) are limited to the home-based trips
# They exclude nonhome-based trips, school trips and college trips
Pr <- c("hbw", "hbs", "hbr", "hbo")

# Define the travel modes
Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")

# Define time period 
Tp <- c("peak", "offpeak")  

# Define calculate method 
Cm <- c("min", "weighted")

# define mode cost transformation coefficient (% of hourly wage)
modecosttrans.Md <- c(driveAlone = 0.5, drivePass=0.5, pass=0.35, busWalk=0.35, parkAndRideBus=0.35, bike=0.5, walk=0.5)

# Load zone abbreviation 
load("data/CenTTime/CenRdata/Zi.RData")




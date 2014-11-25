#This script prepares the workspace and file directories for calculating travel time and travel cost through employment centres methods 

# set workplace and file directories 
setwd("C:\\Users\\huajie\\Desktop\\OHASTTime")

# Define income group abbreviation
Ic <- c("lowInc", "midInc", "highInc")

# Define trip purpose abbreviation
# The purposes for this study (now) are limited to the home-based trips
# They exclude nonhome-based trips, school trips and college trips
Pr <- c("hbw", "hbs", "hbr", "hbo")

# Load zone abbreviation 
load("Rdata/Zi.RData")
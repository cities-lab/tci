# This script prepares the workspace and file directories for calculating 
# travel time and travel cost with OHAS data

# Set workspace
setwd("~/tci")

# Define income group abbreviation
Ic <- c("lowInc", "midInc", "highInc")

# Define trip purpose abbreviation
# The purposes for this study (now) are limited to the home-based trips
# They exclude nonhome-based trips, school trips and college trips
Pr <- c("hbw", "hbs", "hbr", "hbo")

#Define income group category index
IncomeCoeff.Ic <- c(lowInc=1,midInc=2,highInc=3)

# Load zone abbreviation 
load("data/CommonData/Zi.RData")
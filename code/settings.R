# settings that are common to all methods

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

#from 2013 Trip-Based Travel Demand Model Methodology Report
Tp.factors <- data.frame(pr=c(),
                         md=c(),
                         tp=c(),
                         factor=c()
)

# Define calculate method 
Cm <- c("min", "weighted")


# unit travel costs by mode
# unit travel costs can be differentiated by income 
# by adding an "inc.level" column to the data.frame
# with value "lowInc", "midInc", "highInc"

# # this configuration converts travel costs to $
# hourly.wage <- 60 
# #hourly.wage <- 24.77
# MODE <- c(1:10,97, 21, 22, 23, 24, 25) # 1:10 and 97 are coded in OHAS; 1:2 and 21:25 are coded for TDM
# MdNames <- c("walk", "bike", "auto / van/ truck driver", "auto / van / truck passenger", "bus", "rail", "dial-a-ride/paratransit", "taxi",  
#              "school bus", "carpool / vanpool", "other (specify)", "driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus")
# names(MODE) <- MdNames
# 
# #1  WALK
# #2  BIKE
# #3	AUTO / VAN/ TRUCK DRIVER
# #4	AUTO / VAN / TRUCK PASSENGER
# #5	BUS
# #6	RAIL
# #7	DIAL-A-RIDE/PARATRANSIT
# #8	TAXI
# #9	SCHOOL BUS
# #10	CARPOOL / VANPOOL
# #97	OTHER (SPECIFY)
# 
# 
# # distance-based monetary cost per mile
# #http://newsroom.aaa.com/tag/driving-cost-per-mile/
# #http://www.portlandfacts.com/cost_of_transit_&_cars.html
# #http://portlandtaxi.net/rates.php
# #mcpm <- c(0,   0,   59.2,59.2, 101.0, 138.0, 0,  260.0,  0,  0,  29.6, 59.2, 59.2, 59.2, 101.0, 101.0) / 100
# #mcpm <- rep(0, length(MODE))
# 
# ## Alternatively the below configuration converts travel costs to time (minutes or hours)
# #MODE <- c(1:10,97) #as being coded in OHAS
# VOT <- rep(1, length(MODE)) * hourly.wage
# # VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35) * hourly.wage
# 
# ## time-equivalent monetary cost per mile, which can be specific to income group
#  mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0) *60/ (100 * 24.77)
# # mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0) / 100
# 
# unitcosts <- data.frame(MODE, VOT, mcpm)

# Define unit cost 

# Define OHAS unit costs 
  MODE <- c(1:10,97, 21, 22, 23, 24, 25) # 1:10 and 97 are coded in OHAS; 1:2 and 21:25 are coded for TDM
  MdNames <- c("walk", "bike", "auto / van/ truck driver", "auto / van / truck passenger", "bus", "rail", "dial-a-ride/paratransit", "taxi",  
                "school bus", "carpool / vanpool", "other (specify)", "driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus")
  names(MODE) <- MdNames
  
  # unit cost by minutes 
  hourly.wage <- 60 
  VOT <- rep(1, length(MODE)) * hourly.wage
  mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0)*60 / (100 * 24.77)
  unitcosts.minutes <- data.frame(MODE, VOT, mcpm, unit="minutes")
  
  # unit cost by dollars 
  hourly.wage <- 24.77
  VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35) * hourly.wage
  mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0) / 100
  unitcosts.dollars <- data.frame(MODE, VOT, mcpm, unit="dollars")
  
  # Combine unitcosts of different units   
  OHAS.unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)


# define unit cost for Portland 1994 survey data
# distance-based monetary cost per mile
# http://www.portlandfacts.com/cost_of_transit_&_cars.html
# http://portlandtaxi.net/rates.php
## time-equivalent monetary cost per mile, which can be specific to income group
  
  # mode
  MODE <- c(1:8) 
  MdNames <- c("other", "walk", "bicycle", "schol bus", "public bus", "MAX", "personal vehicle", "non-personal vehicle") 
  names(MODE) <- MdNames
  
  # minutes unit cost 
  hourly.wage <- 60
  VOT <- rep(1, length(MODE)) * hourly.wage
  mcpm <- c(29.6, 0, 0, 0, 101.0, 138.0, 59.2, 59.2)*60 / (100 * 24.77)
  unitcosts.minutes <- data.frame(MODE, VOT, mcpm, unit="minutes")
  
  # dollars unit cost 
  hourly.wage <- 24.77
  VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.5, 0.5) * hourly.wage
  mcpm <- c(29.6, 0, 0, 0, 101.0, 138.0, 59.2, 59.2) / 100
  unitcosts.dollars <- data.frame(MODE, VOT, mcpm, unit="dollars")
  
  # Combine unitcosts of different units   
  Portland94.unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)

# Define unit costs for WFRC_Saltlake survey data
  # mode
  MODE <- c(1:6)
  MdNames <- c("Auto/truck/motorcycle", "Transit", "Walked/wheelchair", "Bicycle", "Other", "School bus (only in independent child trips)") 
  names(MODE) <- MdNames
  
  # unit cost by minutes
  hourly.wage <- 60
  VOT <- rep(1, length(MODE)) * hourly.wage
  mcpm <- c(59.2, 101.0, 0, 0, 29.6, 0)*60/(100 * 24.77)
  
  unitcosts.minutes <- data.frame(MODE, VOT, mcpm, unit="minutes")
  
  # unit cost by dollars 
  hourly.wage <- 24.77
  VOT <- c(0.5, 0.35, 0.5, 0.5, 0.5, 0.35) * hourly.wage
  mcpm <- c(59.2, 101.0, 0, 0, 29.6, 0)/100
  
  unitcosts.dollars <- data.frame(MODE, VOT, mcpm, unit="dollars")
  
  WFRC_SaltLake.unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)
  
# Define unist cost for NHTS 2009 survey data 
  # mode
  MODE <- c(-8, -7, -1, 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12, 14, 19, 22, 23, 97)
  MdNames <- c("Don't know", "Refused", "Appropriate skip", "Car", "Van", "SUV", "Pickup truck", "Other truck", "Motorcycle", "Light electric veh (golf cart)", 
               "Local public bus", "Commuter bus", "School bus", "Charter/tour bus", "Shuttle bus", "Taxicab", "Bicycle", "Walk", "Other")
  names(MODE) <- MdNames
  
  # unit costs by minutes
  hourly.wage <- 60
  VOT <- c(NA, NA, NA, rep(1, (length(MODE) -3))) * hourly.wage
  mcpm <- c(NA, NA, NA, 59.2, 59.2, 59.2, 59.2, 59.2, 29.6, 29.6, 101.0, 101.0, 0, 101.0, 0, 260.0, 0, 0, 29.6)*60/(100 * 24.77)
  unitcosts.minutes <- data.frame(MODE, VOT, mcpm, unit="minutes")
  
  # unit costs by dollars 
  hourly.wage <- 24.77
  VOT <- c(NA, NA, NA, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5) * hourly.wage
  mcpm <- c(NA, NA, NA, 59.2, 59.2, 59.2, 59.2, 59.2, 29.6, 29.6, 101.0, 101.0, 0, 101.0, 0, 260.0, 0, 0, 29.6)/100
  
  unitcosts.dollars <- data.frame(MODE, VOT, mcpm, unit="dollars")
  
  NHTS09.unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)  
  
  
# Define directory 
  # setting 
  INPUT_DIR <- 'data/'
  SAVE.INTERMEDIARIES <- TRUE
  INTERMEDIATE_DIR <- file.path("output/intermediate", approach.name, data.source,  unit.name)
  dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)  
  OUTPUT_DIR = file.path("output",approach.name, data.source, unit.name)
  dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)


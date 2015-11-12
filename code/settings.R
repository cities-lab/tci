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
  
  #TOD factors from 2013 Trip-Based Travel Demand Model Methodology Report
  
  
  # Define calculate method 
  Cm <- c("min", "weighted")
  
  unit.name <- ifelse(exists('unit.name'), unit.name, 'dollars')
  #unit.name <- ifelse(exists('unit.name'), unit.name, 'minutes')
  
  scenario.name <- ifelse(exists('scenario.name'), scenario.name, '')
  year <- ifelse(exists('year'), year, '')
  
# define unit travel costs by mode
# unit travel costs may be differentiated by income 
# by adding an "inc.level" column to the data.frame
# with value "lowInc", "midInc", "highInc"

# # this configuration converts travel costs to $
  minutes.per.hour <- 60
  cents.per.dollar <- 100
  #hourly wage in dollars ($/hour)
  hourly.wage <- 24.77
  #minutes that are equivalent to 1 cent monetary costs of travel (minutes/cent)
  #by default, hourly wage is used to get the quantity
  #alternative VOT can be used here
  minutes.per.cent <- minutes.per.hour / (hourly.wage * cents.per.dollar)
  
  stopifnot((100 * hourly.wage) * minutes.per.cent == minutes.per.hour)

## TODO: move this into data
# Sample unit.costs settings
# MODE <- c(1:10,97, 21, 22, 23, 24, 25) # 1:10 and 97 are coded in OHAS; 1:2 and 21:25 are coded for TDM
# MdNames <- c("walk", "bike", "auto / van/ truck driver", "auto / van / truck passenger", "bus", "rail", "dial-a-ride/paratransit", "taxi",  
#              "school bus", "carpool / vanpool", "other (specify)", "driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus")
# names(MODE) <- MdNames
#  
# # #1  WALK
# # #2  BIKE
# # #3	AUTO / VAN/ TRUCK DRIVER
# # #4	AUTO / VAN / TRUCK PASSENGER
# # #5	BUS
# # #6	RAIL
# # #7	DIAL-A-RIDE/PARATRANSIT
# # #8	TAXI
# # #9	SCHOOL BUS
# # #10	CARPOOL / VANPOOL
# # #97	OTHER (SPECIFY)
# # 
# # 
# # # distance-based monetary cost per mile
# # #http://newsroom.aaa.com/tag/driving-cost-per-mile/
# # #http://www.portlandfacts.com/cost_of_transit_&_cars.html
# # #http://portlandtaxi.net/rates.php
#   constant <- rep(0, length(MODE))
# 
#   # unit cost by minutes
#   VOT <- rep(1, length(MODE)) * minutes.per.hour  # hours to minutes
#   # mcpm (monetary cost per mile) in minute/mile
#   # conversion from cent/mile to minute/mile, as cent/mile x minute/cent = minute/mile
#   mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0) * minutes.per.cent
#   
#   unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit.name = "minutes")
#   
#   # unit cost by dollars
#   # VOT in $/hour
#   VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35) * hourly.wage
#   # mcpm in $/mile
#   mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0) / cents.per.dollar
#   unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit.name = "dollars")
#   
#   # Combine unitcosts of different units   
#   OHAS.unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)

# define unit cost for Portland 1994 survey data
# distance-based monetary cost per mile
# http://www.portlandfacts.com/cost_of_transit_&_cars.html
# http://portlandtaxi.net/rates.php
## time-equivalent monetary cost per mile, which can be specific to income group
  
# Define directory 
  # setting
  subdir <- paste0(scenario.name, year)
  project.subdir <- ifelse(nchar(subdir)>0, file.path(project.name, subdir), project.name)
  INPUT_DIR <- file.path('data', project.subdir)
  SAVE.INTERMEDIARIES <- TRUE
  INTERMEDIATE_DIR <- file.path("output/intermediate", project.subdir, method.name, unit.name)
  dir.create(file.path(INTERMEDIATE_DIR), recursive=TRUE, showWarnings = FALSE)
  OUTPUT_DIR = file.path("output", project.subdir, method.name, unit.name)
  dir.create(file.path(OUTPUT_DIR), recursive=TRUE, showWarnings = FALSE)
  
  CLEAN.UP <- ifelse(exists('CLEAN.UP'), CLEAN.UP, FALSE) # clean up memory before exiting
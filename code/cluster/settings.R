##Directories (use defaults in code/settings.R)
  #INPUT_DIR <- 'data/'
  #OUTPUT_DIR <- 'output/cluster'
  #dir.create(file.path(OUTPUT_DIR), recursive = TRUE, showWarnings = FALSE)
  # whether to save intermediate results
  # SAVE.INTERMEDIARIES <- TRUE
  # INTERMEDIATE_DIR <- "output/intermediate/cluster"
  # dir.create(file.path(INTERMEDIATE_DIR), recursive = TRUE, showWarnings = FALSE)

## Define income groups, trip purposes, modes, time period, and cost computation method
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

## Define cutoffs for center identification
  #cutoffs <- data.frame(hbw=c(2500, 1000), hbs=c(102, 1000), hbr=c(1763, 1000), hbo=c(495, 1000))
  #row.names(cutoffs)=c("cutoff.val", 'sum.cutoff.val')
  ## alternatively set percentile cutoffs
  cutoffs <- data.frame(hbw=c(.50, .50), hbs=c(.50, .50), hbr=c(.50, .50), hbo=c(.50, .50))
  row.names(cutoffs) <- c("cutoff.percentile", 'sum.cutoff.percentile')


# Define unit costs for TDM 
  MODE <- c(1:7)
  MdNames <- Md
  names(MODE) <- MdNames
  
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
  constant <- rep(0, length(MODE))
  
  # unit cost by minutes
  VOT <- rep(1, length(MODE)) * minutes.per.hour  # hours to minutes
  # mcpm (monetary cost per mile) in minute/mile
  # conversion from cent/mile to minute/mile, as cent/mile x minute/cent = minute/mile
  mcpm <- c(0, 0, 59.2, 59.2, 59.2, 101.0, 101.0) * minutes.per.cent
  
  unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit.name = "minutes")
  
  # unit cost by dollars
  # VOT in $/hour
  VOT <- c(0.5, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35) * hourly.wage
  # mcpm in $/mile
  mcpm <- c(0, 0, 59.2, 59.2, 59.2, 101.0, 101.0) / cents.per.dollar
  unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit.name = "dollars")
  
  # Combine unitcosts of different units   
  unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)
  
  # Combine unitcosts of different units   
  unitcosts <- unitcosts.list[[unit.name]]


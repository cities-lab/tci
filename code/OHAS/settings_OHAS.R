# Define unit costs for OHAS
MODE <- c(1:10,97, 21, 22, 23, 24, 25) # 1:10 and 97 are coded in OHAS; 1:2 and 21:25 are coded for TDM
MdNames <- c("walk", "bike", "auto / van/ truck driver", "auto / van / truck passenger", "bus", "rail", "dial-a-ride/paratransit", "taxi",  
             "school bus", "carpool / vanpool", "other (specify)", "driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus")
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
mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0) * minutes.per.cent

unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit.name = "minutes")

# unit cost by dollars
# VOT in $/hour
VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.5, 0.35, 0.35, 0.35) * hourly.wage
# mcpm in $/mile
mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6, 59.2, 59.2, 59.2, 101.0, 101.0) / cents.per.dollar
unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit.name = "dollars")

# Combine unitcosts of different units   
unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)

# Combine unitcosts of different units   
unitcosts <- unitcosts.list[[unit.name]]
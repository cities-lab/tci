# Define unit costs for OHAS
# Use ThisMODE field in linkedTrip dataset 
MODE <- c("WALK", "BIKE", "DRIVER", "PASSENGER", "BUS", "RAIL", "PARATRANSIT", "TAXI",  
          "SCHOOLBUS", "CAR/VANPOOL", "OTHER")

# # distance-based monetary cost per mile
# #http://newsroom.aaa.com/tag/driving-cost-per-mile/
# #http://www.portlandfacts.com/cost_of_transit_&_cars.html
# #http://portlandtaxi.net/rates.php
constant <- rep(0, length(MODE))

# unit cost by minutes
VOT <- rep(1, length(MODE)) * minutes.per.hour  # hours to minutes
# mcpm (monetary cost per mile) in minute/mile
# conversion from cent/mile to minute/mile, as cent/mile x minute/cent = minute/mile
mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6) * minutes.per.cent

unitcosts.minutes <- data.frame(MODE, constant, VOT, mcpm, unit.name = "minutes")

# unit cost by dollars
# VOT in $/hour
VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5) * hourly.wage
# mcpm in $/mile
mcpm <- c(0, 0, 59.2, 59.2, 101.0, 138.0, 0, 260.0, 0, 0, 29.6) / cents.per.dollar
unitcosts.dollars <- data.frame(MODE, constant, VOT, mcpm, unit.name = "dollars")

# Combine unitcosts of different units   
unitcosts.list <- list(minutes=unitcosts.minutes, dollars=unitcosts.dollars)

# Combine unitcosts of different units   
unitcosts <- unitcosts.list[[unit.name]]  

# settings that are common to all methods

# unit travel costs by mode
# unit travel costs can be differentiated by income 
# by adding an "inc.level" column to the data.frame
# with value "lowInc", "midInc", "highInc"

# this configuration converts travel costs to $
hourly.wage <- 24.77
MODE <- c(1:10,97) #as being coded in OHAS
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
VOT <- c(0.5, 0.5, 0.5, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5) * hourly.wage

# distance-based monetary cost per mile
#http://www.portlandfacts.com/cost_of_transit_&_cars.html
#http://portlandtaxi.net/rates.php
#mcpm <- c(0,   0,   59.2,59.2, 101.0, 138.0, 0,  260.0,  0,  0,  29.6) / 100
mcpm <- rep(0, length(MODE))

## Alternatively the below configuration converts travel costs to time (minutes or hours)
#MODE <- c(1:10,97) #as being coded in OHAS
#VOT <- rep(1, length(MODE))
## time-equivalent monetary cost per mile, which can be specific to income group
#mcpm <- c(0,   0,   59.2,59.2, 101.0, 138.0, 0,  260.0,  0,  0,  29.6) / (100 * hourly.wage)

unitcosts <- data.frame(MODE, VOT, mcpm)
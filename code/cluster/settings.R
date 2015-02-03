INPUT_DIR <- 'data/'
OUTPUT_DIR <- 'output/cluster'
dir.create(file.path(OUTPUT_DIR), showWarnings = FALSE)
# whether to save intermediate results
SAVE.INTERMEDIARIES <- TRUE
INTERMEDIATE_DIR <- "output/intermediate/cluster"
dir.create(file.path(INTERMEDIATE_DIR), showWarnings = FALSE)
max.taz_id <- 2162

# Define bike and walk access utility coefficients for travel time (##TODO: no alt specific constants?)
#from 2013 Trip-Based Travel Demand Model Methodology Report


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

# define mode cost transformation coefficient (% of hourly wage)
hourly.wage <- 24.77
modecosttrans.Md <- c(driveAlone = 0.5, drivePass=0.5, pass=0.35, busWalk=0.35, parkAndRideBus=0.35, bike=0.5, walk=0.5)
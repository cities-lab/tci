# mf202.csv is the bike commute distance from MetroSkims2010
# mf204.csv is the walk commute distance from MetroSkims2010
commuteDist<- read.csv("data/CenTTime/CenRdata/csv/mf202.csv", header=FALSE)
noncommuteDist <- read.csv("data/CenTTime/CenRdata/csv/mf204.csv",header=FALSE)

all.equal(commuteDist,noncommuteDist)  # Commute and noncommute distance are the same


# compare
load("data/CenTTime/CenRdata/bikeTimebikehbw.RData")
load("data/CenTTime/CenRdata/bikeTimebikehbo.RData")
load("data/CenTTime/CenRdata/bikeTimebikehbr.RData")
load("data/CenTTime/CenRdata/bikeTimebikehbs.RData")

all.equal(bikeTimebikehbw,bikeTimebikehbo, bikeTimebikehbr,bikeTimebikehbs) # all biketime of four purpose are the same


# mf202.csv file is the bike commute distance from MetroSkims2010
# Bike commute distance is the same as bike non-commute distance in MereoSkim2010 

# Load and save trip distance 
tripDist <- read.csv("data/CenTTime/CenRdata/csv/mf202.csv")
save(tripDist, file="data/CenTTime/CenRdata/tripDist.RData")

# Calculate and save bikeTime and walkTime
bikeTime <- tripDist*60/10
walkTime <- tripDist*60/3

save(bikeTime, file="data/CenTTime/CenRdata/bikeTime.RData")
save(walkTime, file="data/CenTTime/CenRdata/walkTime.RData")

# Thus the line in 5_cal_travel_time.R can change from following line 31, 32 to line 34,35 

# load travel time for bike and walk 
TTimeFileName <- paste("data/CenTTime/CenRdata/", md, "Time",md, pr, ".RData", sep="")

# load travel time for bike and walk 
TTimeFileName <- paste("data/CenTTime/CenRdata/", md, "Time", ".RData", sep="")





# This script load auto travel Distance during peak and offpeak period
source("code/cluster/settings.R")
load(file.path(INPUT_DIR, 'Zi.RData'))

# load auto Distance
# load auto travel time during peak time period 
# mf40  	2a2std	2a2 SOV auto travel Distance
autopeakDistance  <- read.csv(file.path(INPUT_DIR, 'TDM/mf40.csv'), header=F)  
rowindex <- c(1:nrow(autopeakDistance)) # nrow(autopeakDistance) = 4672082 < 2162*2162 = 4674244
driveAlonepeakDistance <- matrix(0, 2162,2162)

for (i in rowindex) {
  
  driveAlonepeakDistance[autopeakDistance[i,1],autopeakDistance[i,2]] <- autopeakDistance[i,3]
  
}


drivePasspeakDistance <- driveAlonepeakDistance
passpeakDistance <- driveAlonepeakDistance

input.file <- file.path(INPUT_DIR, 'TDM/skims/autopeakDistance.RData')
save(driveAlonepeakDistance,drivePasspeakDistance, passpeakDistance, file=input.file)

all.equal(drivePasspeakDistance, drivePassoffpeakDistance)

# load auto travel time during offpeak time period 
# mf42  	3m6std	3m6 SOV auto travel Distance
autooffpeakDistance  <- read.csv(file.path(INPUT_DIR, 'TDM/mf42.csv'), header=F)  
rowindex <- c(1:nrow(autooffpeakDistance))
driveAloneoffpeakDistance <- matrix(0, 2162,2162)
for (i in rowindex) {
  
  driveAloneoffpeakDistance[autooffpeakDistance[i,1],autooffpeakDistance[i,2]] <- autooffpeakDistance[i,3]
  
}

drivePassoffpeakDistance <- driveAloneoffpeakDistance
passoffpeakDistance <- driveAloneoffpeakDistance

input.file <- file.path(INPUT_DIR, 'TDM/skims/autooffpeakDistance.RData')
save(driveAloneoffpeakDistance,drivePassoffpeakDistance, passoffpeakDistance, file=input.file)

# Transit distance
# mf57 description in ttransit skims
# mf57	 2biv    	12-12-04 11:11  22 Bus In-vehicle Time  
IVbuspeakTime <- read.csv(file.path(INPUT_DIR, 'TDM/mf57.csv'), header=F)  
nrowindex <- c(1:nrow(IVbuspeakTime))
IVbuspeakTime.mx <- matrix(NA,2162,2162)
for(i in nrowindex){
  IVbuspeakTime.mx[IVbuspeakTime[i,1],IVbuspeakTime[i,2]]<-IVbuspeakTime[i,3]
}

IVbuspeakTime.mx[IVbuspeakTime.mx==9999] <- NA

busWalkpeakDistance <- IVbuspeakTime.mx*15.2/60
parkAndRideBusDistance <- IVbuspeakTime.mx*15.2/60

# mf47 description
# mf47	 3biv    12-12-04 11:12  33 Bus In-vehicle Time  
IVbusoffpeakTime <- read.csv(file.path(INPUT_DIR, 'TDM/mf47.csv'), header=F)  
nrowindex <- c(1:nrow(IVbusoffpeakTime))
IVbusoffpeakTime.mx <- matrix(NA,2162,2162)
for(i in nrowindex){
  IVbusoffpeakTime.mx[IVbusoffpeakTime[i,1],IVbusoffpeakTime[i,2]]<-IVbusoffpeakTime[i,3]
}

IVbusoffpeakTime.mx[IVbusoffpeakTime.mx==9999] <- NA

busWalkoffpeakDistance <- IVbusoffpeakTime.mx*15.2/60
parkAndRideBusDistance <- IVbusoffpeakTime.mx*15.2/60



# load bus travel Distance: use full transit time to calculate transit distance 

#load(file.path(INPUT_DIR, "TDM/skims", paste("busWalk", "peak", "Time.RData", sep="" )))
#busWalkpeakDistance <- busWalkpeakTime*15.2/60

#load(file.path(INPUT_DIR, "TDM/skims", paste("busWalk", "offpeak", "Time.RData", sep="" )))
#busWalkoffpeakDistance <- busWalkoffpeakTime*15.2/60

#load(file.path(INPUT_DIR, "TDM/skims", paste("parkAndRideBus", "peak", "Time.RData", sep="" )))
#parkAndRideBuspeakDistance <- parkAndRideBuspeakTime*15.2/60

#load(file.path(INPUT_DIR, "TDM/skims", paste("parkAndRideBus", "offpeak", "Time.RData", sep="" )))
#parkAndRideBusoffpeakDistance <- parkAndRideBusoffpeakTime*15.2/60

#input.file <- file.path(INPUT_DIR, 'TDM/skims/busoffpeakDistance.RData')
#save(busWalkpeakDistance, parkAndRideBuspeakDistance, file=input.file)

#input.file <- file.path(INPUT_DIR, 'TDM/skims/busoffoffpeakDistance.RData')
#save(busWalkoffpeakDistance, parkAndRideBusoffpeakDistance, file=input.file)


# bike travel Distance 
# mf202.csv is the bike commute Distance from MetroSkims2010
# mf204.csv is the bike non-commute Distance from MetroSkims2010
# Load TAZ names
load(file.path(INPUT_DIR, 'Zi.RData'))
bikeDist.c <- read.csv(file.path(INPUT_DIR, 'TDM/mf202.csv'), header=FALSE)
bikeDist.nc <- read.csv(file.path(INPUT_DIR, 'TDM/mf204.csv'), header=FALSE)

all.equal(bikeDist.c, bikeDist.nc)  # Commute and noncommute Distance are the same
bdist.mx <- matrix(bikeDist.c[, 3], nrow=max.taz_id, byrow=TRUE,
                   dimnames=list(Zi, Zi))
bikeDistance <- bdist.mx

# 
# Load walk Distance   
wdist.df <- read.table(file.path(INPUT_DIR, 'TDM/2010_wdist.csv'), sep=",", row.names=1, skip=1) #Distance matrix used for calculating walk time
wdist.mx <- as.matrix(wdist.df, dimnames=list(Zi, Zi))
walkDistance <- wdist.mx

# Load functions
source("code/thirdparty/omx.r")
require(rhdf5)

# travel Distance skims
INPUT_DIR = "data/TDM/skims"
# Create a OMX file to store matrix of 2162 * 2162 cells
OUTPUT_DIR = "data/TDM"
out.file <- file.path(OUTPUT_DIR, "DistanceSkims.omx")
createFileOMX(out.file, max.taz_id, max.taz_id, Replace=TRUE)

for (md in Md) {
  if ((md == "bike")|(md=="walk")) {
    # Get travel Distance skims
    DistanceSkimsObjName <- paste(md, "Distance", sep="")
    DistanceSKimsObj <- get(DistanceSkimsObjName)
    
    # Description of matrices
    MatrixDescription <- paste("travel distance skims for", md, sep=" ")
    writeMatrixOMX(out.file, DistanceSKimsObj, DistanceSkimsObjName, Description=MatrixDescription)
  } else {
    for (tp in Tp) {
      DistanceSkimsObjName <- paste(md, tp, "Distance", sep="")
      #in.file <- file.path(INPUT_DIR, paste(DistanceSkimsObjName, ".RData", sep=""))
      #load(in.file); rm(in.file)
      
      DistanceSKimsObj <- get(DistanceSkimsObjName)
      # Description of matrices
      MatrixDescription <- paste(tp, "travel distance skims for", md, sep=" ")
      
      # Write the matrices ##TODO: use Metro's original matrix name
      writeMatrixOMX(out.file, DistanceSKimsObj, DistanceSkimsObjName, Description=MatrixDescription) 
    }
  }
}


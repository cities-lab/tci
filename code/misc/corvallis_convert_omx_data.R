# This script load trip distribution, skims, and access utilities matrices into OMX files

setwd("~/tci")
source("code/cluster/settings.R")
INPUT_DIR <- 'data/'
# Load TAZ names
load(file.path(INPUT_DIR, "Corvallis_cluster/Zi.RData"))
max.taz_id <- length(Zi)

# Load functions
source("code/thirdparty/omx.r")
require(rhdf5)


# trip distribution matrices
INPUT_DIR = "data/Corvallis_cluster/tripdist"
# Create a OMX file to store matrix of 354 * 354 cells
OUTPUT_DIR = "data/Corvallis_cluster/TDM"
out.file <- file.path(OUTPUT_DIR, "TripDistribution.omx")
createFileOMX(out.file, max.taz_id, max.taz_id, Replace=TRUE)

# Begin iteration by trip purpose 
for (pr in Pr) {
  # Begin iteraton by income group  
  for (ic in Ic) {
    # Load trip distribution matrices for each income group
    TripsObjName <- paste(pr, ic, "Dist", sep="")
    in.file <- file.path(INPUT_DIR, paste(TripsObjName, ".RData", sep=""))
    load(in.file); rm(in.file)
    
    # Get trip matrix 
    Trips.ZiZi <- get(TripsObjName)
    Trips.ZiZi <- Trips.ZiZi[Zi, Zi]
    
    # Description of matrices
    MatrixDescription <- paste("origin-destination trip disribution matrix for", ic, 
                               "household group for", pr, "purpose", sep=" ")
    
    # Write the matrices ##TODO: use Metro's original matrix name
    writeMatrixOMX(out.file, Trips.ZiZi, TripsObjName, Description=MatrixDescription)

  # End loop through income groups  
  }
# End loop through trip purpose
}

#begin test
# List the contents of the file including attributes
listOMX(out.file)
# test the resuts of OMX file
testmatrix11 <- readMatrixOMX(out.file, "hbwhighIncDist" )
# stopifnot(all.equal(testmatrix11[1:5,1:5], hbwhighIncDist[Zi, Zi][1:5,1:5]))
testmatrix11[1:5,1:5]==hbwhighIncDist[Zi, Zi][1:5,1:5]
#end test

# travel time skims
INPUT_DIR = "data/Corvallis_cluster/input/RData"
# Create a OMX file to store matrix of 354 * 354 cells
OUTPUT_DIR = "data/Corvallis_cluster/TDM"
out.file <- file.path(OUTPUT_DIR, "Skims.omx")
createFileOMX(out.file, max.taz_id, max.taz_id, Replace=TRUE)

# Load data
# Travel time by mode formulas (input/utilities.hbrAccessUtils.csv) 
# driveAlone time 
load(file.path(INPUT_DIR, "ivTimeoffPeakdriveAlone.RData"))
load(file.path(INPUT_DIR, "ivTimepeakdriveAlone.RData"))
load(file.path(INPUT_DIR, "walkTimedriveAlone.RData")) # walk time of driveAlone does not differ by time period 

driveAlonepeakTime <- ivTimepeakdriveAlone + walkTimedriveAlone
driveAloneoffpeakTime <- ivTimeoffPeakdriveAlone + walkTimedriveAlone

# drivePass 
load(file.path(INPUT_DIR, "ivTimeoffPeakdrivePass.RData"))
load(file.path(INPUT_DIR, "ivTimepeakdrivePass.RData"))
load(file.path(INPUT_DIR, "walkTimedrivePass.RData"))

drivePasspeakTime <- ivTimepeakdrivePass + walkTimedrivePass
drivePassoffpeakTime <- ivTimeoffPeakdrivePass + walkTimedrivePass

# pass 
load(file.path(INPUT_DIR, "ivTimeoffPeakpass.RData"))
load(file.path(INPUT_DIR, "ivTimepeakpass.RData"))
load(file.path(INPUT_DIR, "walkTimepass.RData"))

passpeakTime <- ivTimepeakpass + walkTimepass
passoffpeakTime <- ivTimeoffPeakpass + walkTimepass

# busWalk
load(file.path(INPUT_DIR, "ivTimeoffPeakbusWalk.RData"))
load(file.path(INPUT_DIR, "ivTimepeakbusWalk.RData"))
load(file.path(INPUT_DIR, "walkTimeoffPeakbusWalk.RData"))
load(file.path(INPUT_DIR, "walkTimepeakbusWalk.RData"))
load(file.path(INPUT_DIR, "waitTimeAoffPeakbusWalk.RData"))
load(file.path(INPUT_DIR, "waitTimeApeakbusWalk.RData"))
load(file.path(INPUT_DIR, "waitTimeBoffPeakbusWalk.RData"))
load(file.path(INPUT_DIR, "waitTimeBpeakbusWalk.RData"))
load(file.path(INPUT_DIR, "boardingsoffPeakbusWalk.RData"))
load(file.path(INPUT_DIR, "boardingspeakbusWalk.RData"))

busWalkoffpeakTime <- ivTimeoffPeakbusWalk + walkTimeoffPeakbusWalk + waitTimeAoffPeakbusWalk + waitTimeBoffPeakbusWalk # + boardingsoffPeakbusWalk
busWalkpeakTime <- ivTimepeakbusWalk + walkTimepeakbusWalk + waitTimeApeakbusWalk + waitTimeBpeakbusWalk # + boardingspeakbusWalk

# parkAndRideBus
load(file.path(INPUT_DIR, "ivTimeoffPeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "ivTimepeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "walkTimeoffPeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "walkTimepeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "waitTimeAoffPeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "waitTimeApeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "waitTimeBoffPeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "waitTimeBpeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "boardingsoffPeakparkAndRideBus.RData"))
load(file.path(INPUT_DIR, "boardingspeakparkAndRideBus.RData"))

parkAndRideBusoffpeakTime <- ivTimeoffPeakparkAndRideBus + walkTimeoffPeakparkAndRideBus + waitTimeAoffPeakparkAndRideBus + waitTimeBoffPeakparkAndRideBus # + boardingsoffPeakparkAndRideBus
parkAndRideBuspeakTime <- ivTimepeakparkAndRideBus + walkTimepeakparkAndRideBus + waitTimeApeakparkAndRideBus + waitTimeBpeakparkAndRideBus # + boardingspeakparkAndRideBus

# Bike
load(file.path(INPUT_DIR, "bikeTime.RData"))

# Walk 
load(file.path(INPUT_DIR, "walkTimewalk.RData"))
walkTime <- walkTimewalk

for (md in Md) {
    if ((md == "bike")|(md=="walk")) {
      # Get travel time skims
      SkimsObjName <- paste(md, "Time", sep="")
      SKimsObj <- get(SkimsObjName)
      SKimsObj <- SKimsObj[Zi, Zi]
      
      # Description of matrices
      MatrixDescription <- paste("travel time skims for", md, sep=" ")
      writeMatrixOMX(out.file, SKimsObj, SkimsObjName, Description=MatrixDescription)
    } else {
      for (tp in Tp) {
        SkimsObjName <- paste(md, tp, "Time", sep="")
        #in.file <- file.path(INPUT_DIR, paste(SkimsObjName, ".RData", sep=""))
        #load(in.file); rm(in.file)
        
        SKimsObj <- get(SkimsObjName)
        SKimsObj <- SKimsObj[Zi, Zi]
        # Description of matrices
        MatrixDiscrition <- paste(tp, "travel time skims for", md, sep=" ")
        
        # Write the matrices ##TODO: use Metro's original matrix name
        writeMatrixOMX(out.file, SKimsObj, SkimsObjName, Description=MatrixDiscrition) 
      }
    }
}

#begin test
# List the contents of the file including attributes
listOMX(out.file)
# test the resuts of OMX file
testmatrix11 <- readMatrixOMX(out.file, "walkTime" )
# stopifnot(all.equal(testmatrix11[1:10,1:10], walkTime[Zi, Zi][1:10,1:10], na.rm=TRUE))
testmatrix11[1:10,1:10]==walkTime[Zi, Zi][1:10,1:10]
#end test

# mode choice utilties matrices
## TODO: load from the original source
INPUT_DIR = "data/Corvallis_cluster/access"
# Convert mode utility data into OMX file
OUTPUT_DIR = "data/Corvallis_cluster/TDM"
out.file = file.path(OUTPUT_DIR, "ModeUtilities.omx")
# Create a OMX file to store 354 * 354 
createFileOMX(out.file, max.taz_id, max.taz_id, Replace=TRUE)

# Begin iteration by trip purpose 
for (pr in Pr) {
# Begin iteration by mode
  for (md in Md) {
    if ((md == "bike")|(md=="walk")) {
      # Load mode utility data
      in.file <- file.path(INPUT_DIR, paste("util", md, pr, ".RData", sep=""))
      load(in.file) ; rm(in.file)
        
      # Get mode utility
      ModeUtilObjName <- paste("util", md, pr, sep="")
      ExpUtil.ZiZi <- get(ModeUtilObjName)
      ExpUtil.ZiZi <- ExpUtil.ZiZi[Zi, Zi]
      # Description of matrices
      MatrixDescription <- paste(md, "utility for ", pr, " purpose", sep=" ")
      writeMatrixOMX(out.file, ExpUtil.ZiZi, ModeUtilObjName, Description=MatrixDescription)
      
    # End loop through bike and walk transportation mode     
    } else {
      # Begion iteration by income group
      for (ic in Ic) {
          # Load mode utility data
          ## TODO: load from the original source
          in.file <- file.path(INPUT_DIR, paste("util", md, ic, pr, ".RData", sep=""))
          load(in.file) ; rm(in.file)
          
          # Get mode utility 
          ModeUtilObjName <- paste("util", md, ic, pr, sep="")
          ExpUtil.ZiZi <- get(ModeUtilObjName)
          ExpUtil.ZiZi <- ExpUtil.ZiZi[Zi, Zi]
          
          # Description of matrices
          MatrixDescription <- paste(md, "utility of", ic, "household group for", pr, "purpose", sep=" ")
          
          writeMatrixOMX(out.file, ExpUtil.ZiZi, ModeUtilObjName, Description=MatrixDescription)
          
        # End loop through income group
        }
      # End loop through driveAlone, drivePass, pass, busWalk, parkAndRideBus
      }
    # End loop through trip mode
    }
# End loop through trip purpose
}

# List the contents of the file including attributes
listOMX(out.file)
# test the resuts of OMX file
testmatrix11 <- readMatrixOMX(out.file, "utilpasshighInchbw" )
# stopifnot(all.equal(testmatrix11[1:10,1:10], utilpasshighInchbw[Zi, Zi][1:10,1:10]))
testmatrix11[1:5,1:5] == utilpasshighInchbw[Zi, Zi][1:5,1:5]


# Convert travel distance to OMX 
# travel Distance skims
INPUT_DIR = "data/Corvallis_cluster/input/RData"
# Create a OMX file to store matrix of 354 * 354 cells
OUTPUT_DIR = "data/Corvallis_cluster/TDM"
out.file <- file.path(OUTPUT_DIR, "DistanceSkims.omx")
createFileOMX(out.file, max.taz_id, max.taz_id, Replace=TRUE)

# Scripts from "Corvallis_cluster/inputsSave" show mode distance 
# load("inputs/RData/autoDist.RData")

for (md in Md) {
  if ((md == "bike")|(md=="walk")) {
    
    # Load mode travel distance data
    in.file <- file.path(INPUT_DIR, paste(md, "Dist.RData", sep=""))
    load(in.file) ; rm(in.file)
    
    # Get travel Distance skims
    DistanceSkimsObjName <- paste(md, "Distance", sep="")
    DistanceSKimsObj <- get(paste(md, "Dist", sep=""))
    DistanceSKimsObj <- DistanceSKimsObj[Zi, Zi]
    # Description of matrices
    MatrixDescription <- paste("travel distance skims for", md, sep=" ")
    writeMatrixOMX(out.file, DistanceSKimsObj, DistanceSkimsObjName, Description=MatrixDescription)
  } else if ((md == "driveAlone")|(md=="drivePass")|(md=="pass")) {
    
    # Load mode travel distance data
    in.file <- file.path(INPUT_DIR, paste("autoDist.RData", sep=""))
    load(in.file) ; rm(in.file)
    
    DistanceSkimsObjName <- paste(md, "Distance", sep="")
    DistanceSKimsObj <- get("autoDist")
    DistanceSKimsObj <- DistanceSKimsObj[Zi, Zi]
    
    # Description of matrices
    MatrixDescription <- paste("travel distance skims for", md, sep=" ")
    
    # Write the matrices ##TODO: use Metro's original matrix name
    writeMatrixOMX(out.file, DistanceSKimsObj, DistanceSkimsObjName, Description=MatrixDescription) 
    
  } else if ((md == "busWalk")|(md=="parkAndRideBus")) {
    
    # Load mode travel distance data
    in.file <- file.path(INPUT_DIR, paste("tranDist.RData", sep=""))
    load(in.file) ; rm(in.file)
    
    DistanceSkimsObjName <- paste(md, "Distance", sep="")
    DistanceSKimsObj <- get("tranDist")
    DistanceSKimsObj <- DistanceSKimsObj[Zi, Zi]
    
    # Description of matrices
    MatrixDescription <- paste("travel distance skims for", md, sep=" ")
    
    # Write the matrices ##TODO: use Metro's original matrix name
    writeMatrixOMX(out.file, DistanceSKimsObj, DistanceSkimsObjName, Description=MatrixDescription) 
    
  }
}


# List the contents of the file including attributes
listOMX(out.file)
# test the resuts of OMX file
testmatrix11 <- readMatrixOMX(out.file, "drivePassDistance")
# stopifnot(all.equal(testmatrix11[1:10,1:10], utilpasshighInchbw[Zi, Zi][1:10,1:10]))
testmatrix11[1:5,1:5] == autoDist[Zi, Zi][1:5,1:5]



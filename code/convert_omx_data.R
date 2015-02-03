# This script load trip distribution, skims, and access utilities matrices into OMX files

setwd("~/tci")

source("code/cluster/settings.R")
# Load functions
source("code/thirdparty/omx.r")
require(rhdf5)

#Calculate utilities and travel time for bike & walk
source("code/misc/calculate_bike_walk_utils_time.R")

# trip distribution matrices
INPUT_DIR = "data/TDM/tripdist"
# Create a OMX file to store matrix of 2162 * 2162 cells
OUTPUT_DIR = "data/TDM"
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
stopifnot(all.equal(testmatrix11[1:10,1:10], hbwhighIncDist[1:10,1:10]))
#end test

# travel time skims
INPUT_DIR = "data/TDM/skims"
# Create a OMX file to store matrix of 2162 * 2162 cells
OUTPUT_DIR = "data/TDM"
out.file <- file.path(OUTPUT_DIR, "Skims.omx")
createFileOMX(out.file, max.taz_id, max.taz_id, Replace=TRUE)

for (md in Md) {
    if ((md == "bike")|(md=="walk")) {
      # Get travel time skims
      SkimsObjName <- paste(md, "Time", sep="")
      SKimsObj <- get(SkimsObjName)
      
      # Description of matrices
      MatrixDiscrition <- paste("travel time skims for", md, sep=" ")
      writeMatrixOMX(out.file, SKimsObj, SkimsObjName, Description=MatrixDescription)
    } else {
      for (tp in Tp) {
        SkimsObjName <- paste(md, tp, "Time", sep="")
        in.file <- file.path(INPUT_DIR, paste(SkimsObjName, ".RData", sep=""))
        load(in.file); rm(in.file)
        
        SKimsObj <- get(SkimsObjName)
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
testmatrix11 <- readMatrixOMX(out.file, "busWalkoffpeakTime" )
stopifnot(all.equal(testmatrix11[1:10,1:10], busWalkoffpeakTime[1:10,1:10]))
#end test

# mode choice utilties matrices
## TODO: load from the original source
INPUT_DIR = "data/TCIPortland50/performancemeasures/intm_output/access"
# Convert mode utility data into OMX file
out.file = file.path(OUTPUT_DIR, "TDM/ModeUtilities.omx")
# Create a OMX file to store 2162 * 2162 
createFileOMX(out.file, max.taz_id, max.taz_id)

# Begin iteration by trip purpose 
for (pr in Pr) {
# Begin iteration by mode
  for (md in Md) {
    if ((md == "bike")|(md=="walk")) {
      # Load mode utility data
      #in.file <- file.path(INPUT_DIR, paste("util", md, pr, ".RData", sep=""))
      #load(in.file) ; rm(in.file)
        
      # Get mode utility
      ModeUtilObjName <- paste("util", md, pr, sep="")
      ExpUtil.ZiZi <- get(ModeUtilObjName)
      
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
stopifnot(all.equal(testmatrix11[1:10,1:10], utilpasshighInchbw[1:10,1:10]))

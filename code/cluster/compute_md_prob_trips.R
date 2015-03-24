# This script calculates modes probabilities and number of trips for all modes 

#Load the rhdf5 library
library(rhdf5)

# Load functions for OMX files 
source("code/thirdparty/omx.r")
H5close() # close any hdf5 file handle that may still be open

in.file_tripdistribution <- file.path(INPUT_DIR, 'TDM/TripDistribution.omx')
in.file_modeutils <- file.path(INPUT_DIR, "TDM/ModeUtilities.omx")
# Create a OMX file to store 2162 * 2162 matrices of trips by mode
if (SAVE.INTERMEDIARIES) {
  intm.file_modetrips = file.path(INTERMEDIATE_DIR, "ModeTrips.omx")
  createFileOMX(intm.file_modetrips, max.taz_id, max.taz_id, Replace=TRUE)
}

load(file.path(INPUT_DIR, 'Zi.RData'))
if (!in.memory(paste(Pr, "ci", sep="")))
  load(file.path(INTERMEDIATE_DIR, "centers.RData"))

# Calculate travel time by bike and walk from bike/walk trip distance
# This is now done in a separate script misc/calculate_bike_walk_utils_time.R

#Begin iteration by trip purpose
#-------------------------------
for(pr in Pr){ 
  CentersObjName <- paste(pr, "ci", sep="")
  Centers <- get(CentersObjName); rm(CentersObjName)
  
  # Begin iteration by income group
  for(ic in Ic){
    # Initialize an array to hold all the mode utility data           
    ExpUtils.ZiZiMd <- array(0, dim=c(length(Zi), length(Zi), length(Md)), 
                             dimnames=list(Zi, Zi, Md))
    
    # Populate the array with the mode utility data
    for(md in Md){
      # Load the array of zone to zone utilities and assign to Util.ZiZi
      if ((md == "bike") | (md=="walk")) {
        #
        UtilObjName <- paste("util", md, pr, sep="")
      } else {
        #
        UtilObjName <- paste("util", md, ic, pr, sep="")
      }
      
      ExpUtil.ZiZi <- readMatrixOMX(in.file_modeutils, UtilObjName)
      
      rm(list=ls()[ls()==UtilObjName]) ; rm(UtilObjName)
      dimnames(ExpUtil.ZiZi) <- list(Zi, Zi)
      
      # Add the mode matrix to the array
      ExpUtils.ZiZiMd[,,md] <- ExpUtil.ZiZi ; rm(ExpUtil.ZiZi)
    }
    
    # Correct problem with utility for short walk and bike trips
    # Setting everything greater than 1 to 1 makes a minimum
    # trip time of approximately X minutes
    ExpUtils.ZiZiMd[ExpUtils.ZiZiMd > 0.99] <- 0.99
    
    # Calculate the mode probabilities for all modes
    ModeProbs.ZiZiMd <- sweep(ExpUtils.ZiZiMd, c(1,2),
                              apply(ExpUtils.ZiZiMd, c(1,2), sum), "/")
    obj.name <- paste(pr, ic, "ModeProbs.ZiZiMd", sep="")
    assign(obj.name, ModeProbs.ZiZiMd)
    if (SAVE.INTERMEDIARIES) {
      intm.path = file.path(INTERMEDIATE_DIR, 'modeprobs')
      dir.create(intm.path, recursive = TRUE, showWarnings = FALSE)
      intm.file = file.path(intm.path, paste(obj.name, ".RData", sep=""))
      save(list=obj.name, file=intm.file)
    }
    
    # Load trip distribution matrices for each trip purpose and income group
    TripsObjName <- paste(pr, ic, "Dist", sep="")
    Trips.ZiZi <- readMatrixOMX(in.file_tripdistribution, TripsObjName)
    
    # Define array to store trips by mode
    Trips.ZiZiMd <- array(0, dim=c(length(Zi), length(Zi), length(Md)), 
                          dimnames=list(Zi, Zi, Md))
    TotTrips.ZiMd <- array(0, dim=c(length(Zi), length(Md)), 
                           dimnames=list(Zi, Md))
    
    # Calculate the trips by mode
    for(md in Md){
      TripsMd <- Trips.ZiZi * ModeProbs.ZiZiMd[,,md]
      
      obj.name <- paste(pr, ic, md, "trips", sep="")
      assign(obj.name, TripsMd)
            
      # combine trips into array
      Trips.ZiZiMd[,,md] <- TripsMd
      TotTrips.ZiMd[,md] <- rowSums(TripsMd[,Centers$TAZ])
      
      if (SAVE.INTERMEDIARIES) {
        # Description of trip matrices
        MatrixDiscription <- paste("origin-destination trips table of", ic, "household group for", pr, "purpose by", md, sep=" ")
        # Write the matrices
        writeMatrixOMX(intm.file_modetrips, TripsMd, obj.name, Description=MatrixDiscription)
      }
      
      rm(TripsMd)
    } # End loop through modes
    
    obj.name <- paste(pr, ic, "TotTrips.ZiMd", sep="")
    assign(obj.name, TotTrips.ZiMd) 
    if (SAVE.INTERMEDIARIES) {
      intm.path = file.path(INTERMEDIATE_DIR, 'trips')
      dir.create(intm.path, recursive = TRUE, showWarnings = FALSE)
      intm.file = file.path(intm.path, paste(obj.name, ".RData", sep=""))
      save(list=obj.name, file = intm.file)
    }
    
  } # End loop through income groups
} # End loop through purposes

# Define income group abbreviation
  Ic <- c("lowInc", "midInc", "highInc")

# Define trip purpose abbreviation
# The purposes for this study (now) are limited to the home-based trips
# They exclude nonhome-based trips, school trips and college trips
  Pr <- c("hbw", "hbs", "hbr", "hbo")

# Define the travel modes
  Md <- c("driveAlone", "drivePass", "pass", "busWalk", "parkAndRideBus", "bike", "walk")

# Load functions
  source("code/omx.r")

# Convert trip distribution matirces into OMX file

  # Create a OMX file to store 2162 * 2162 
    createFileOMX("data/CommonData/OMX/TripDistribution.omx", 2162, 2162)

  # Begin iteration by trip purpose 

    for (pr in Pr) {
      
        # Begin iteraton by income group  
        for (ic in Ic) {
    
          # Load trip distribution matrices for each income group
    
            DistFileName <- paste("data/CommonData/tripdist/", pr, ic, "Dist.RData", sep="")
            load(DistFileName); rm(DistFileName)
    
          # Get trip matrix 
            TripsObjName <- paste(pr, ic, "Dist", sep="")
            Trips.ZiZi <- get(TripsObjName)
    
          # Description of matrices
            FileDiscrition <- paste("origin-destination trip disribution matrix of", ic, "household group for", pr, "purpose", sep=" ")
    
          # Write the matrices
            writeMatrixOMX("data/CommonData/OMX/TripDistribution.omx", Trips.ZiZi, TripsObjName, 
                          Description=FileDiscrition)
            
          # End loop through income groups  
        }
  
       # End loop through trip purpose
    }


  # List the contents of the file including attributes
    listOMX("data/CommonData/OMX/TripDistribution.omx")


  # test the resuts of OMX file
    testmatrix11 <- readMatrixOMX("data/CommonData/OMX/TripDistribution.omx", "hbwhighIncDist" )
    testmatrix11[1:10,1:10]
    hbwhighIncDist[1:10,1:10]


# Convert mode utility data into OMX file 
  
  # Create a OMX file to store 2162 * 2162 
  createFileOMX("data/CommonData/OMX/ModeUtility.omx", 2162, 2162)
  
  # Begin iteration by trip purpose 
  
  for (pr in Pr) {
    
    # Begin iteration by mode
    for (md in Md) {
      
      if ((md == "bike")|(md=="walk")) {
        
        # Load mode utility data
        ModeUtilFileName <- paste("data/TCIPortland50/performancemeasures/intm_output/access/util", md, pr, ".RData", sep="")
        load(ModeUtilFileName) ; rm(ModeUtilFileName)
        
        # Get mode utility 
        ModeUtilObjName <- paste("util", md, pr, sep="")
        ExpUtil.ZiZi <- get(ModeUtilObjName)
        
        # Description of matrices
        FileDiscrition <- paste(md, "utility for", pr, "purpose", sep=" ")
        
        writeMatrixOMX("data/CommonData/OMX/ModeUtility.omx", ExpUtil.ZiZi, ModeUtilObjName, 
                       Description=FileDiscrition)
        
        # End loop through bike and walk transportation mode 
        
      }
      
      if ((md != "bike") & (md != "walk")) {
        
        # Begion iteration by income group
        for (ic in Ic) {
          
          # Load mode utility data
          ModeUtilFileName <- paste("data/TCIPortland50/performancemeasures/intm_output/access/util", md,ic, pr, ".RData", sep="")
          load(ModeUtilFileName) ; rm(ModeUtilFileName)
          
          # Get mode utility 
          ModeUtilObjName <- paste("util", md, ic, pr, sep="")
          ExpUtil.ZiZi <- get(ModeUtilObjName)
          
          # Description of matrices
          FileDiscrition <- paste(md, "utility of", ic, "household group for", pr, "purpose", sep=" ")
          
          writeMatrixOMX("data/CommonData/OMX/ModeUtility.omx", ExpUtil.ZiZi, ModeUtilObjName, 
                         Description=FileDiscrition)
          
          # End loop through income group
        }
        
        # End loop through driveAlone, drivePass, pass, busWalk, parkAndRideBus
      }
      
      
      # End loop through trip mode
    }
    
    
    
    # End loop through trip purpose
  }
  
  
  # List the contents of the file including attributes
  listOMX("data/CommonData/OMX/ModeUtility.omx")
  
  
  # test the resuts of OMX file
  testmatrix11 <- readMatrixOMX("data/CommonData/OMX/ModeUtility.omx", "utilpasshighInchbw" )
  testmatrix11[1:10,1:10]
  utilpasshighInchbw[1:10,1:10]










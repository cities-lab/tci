# This script load emmebank 1994 and emmebank 2011 full matrix into omx
# Convert matrix data into omx 
# Source omx functions
source("code/thirdparty/omx.r")
require(rhdf5)

# Load emmebank 1994 data 
  load("data/emme1994MfNames.RData")
  load("data/emme1994.RData")

  # Define maximum TAZ id 
  max.taz_id <- 1400
  createFileOMX("data/emme1994.omx", max.taz_id, max.taz_id, Replace=TRUE)
  
  str(emme1994MfNames)
  
  for (i in c(1:86)) {
    
    mfName <- emme1994MfNames[i,1]
    mfObj <- get(mfName)
    
    mfDescription <- emme1994MfNames[i,2]
    writeMatrixOMX("data/emme1994.omx",  mfObj, mfName, Description=mfDescription)
    
  }
  
  # test the results 
  rhwda_test <- readMatrixOMX("data/emme1994.omx", "rhwda")
  rhwda_test[1:10,1:5]
  
  rm(list=ls())
  
  # Load emmebank 2011 full matrix into omx
  
  # Load emmebank 2011 data 
  load("data/emme2011MfNames.RData")
  load("data/emme2011.RData")
  
  # Convert matrix data into omx 
  # Source omx functions
  source("code/thirdparty/omx.r")
  require(rhdf5)
  
  
  # Define maximum TAZ id 
  max.taz_id <- 2200
  createFileOMX("data/emme2011.omx", max.taz_id, max.taz_id, Replace=TRUE)
  
  str(emme2011MfNames)
  
  for (i in c(1:211)) {
    
    mfName <- paste("mf", i, sep="")
    mfObj <- get(mfName)
    
    mfDescription <- emme2011MfNames[i,2]
    writeMatrixOMX("data/emme2011.omx",  mfObj, mfName, Description=mfDescription)
    
  }
  
  # test the results 
  mf2_test <- readMatrixOMX("data/emme2011.omx", "mf2")
  mf2_test[1:10,1:5]
  mf2[1:10,1:5]
  
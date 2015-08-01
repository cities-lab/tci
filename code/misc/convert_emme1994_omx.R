# This script load emmebank 1994 full matrix into omx

# Load emmebank 1994 data 
  load("data/emme1994MfNames.RData")
  load("data/emme1994.RData")

  # Convert matrix data into omx 
  # Source omx functions
  source("code/thirdparty/omx.r")
  require(rhdf5)
  
  
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
  
  
  
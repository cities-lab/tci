# This script calculates modes probabilities and number of trips for all modes 

# Define bike and walk access utility coefficients
BikeAccessCoeff.Pr <- c(hbw=-3.217, hbs=-1.839, hbr=-1.839, hbo=-1.839)
WalkAccessCoeff.Pr <- c(hbw=-4.389, hbs=-2.532, hbr=-2.532, hbo=-2.532)

# Load trip distance matrix
load(file.path(INPUT_DIR, "TDM/trip_distance.RData"))
load(file.path(INPUT_DIR, "Zi.RData"))
dimnames(tripDist) <- list(Zi,Zi)
tripDist.ZiZi <- tripDist[Zi,Zi]

#Begin iteration by trip purpose
#-------------------------------

#::
if (!all(c("hbwci", "hbsci", "hbrci", "hboci") %in% ls())) 
  load(file.path(INTERMEDIATE_DIR, "centers.RData"))

for(pr in Pr){ 
  # load taz index of center
#  CentersObjName <- paste(pr,"ci", sep="")
#  Centers <- get(CentersObjName); rm(CentersObjName)
  
  # Begin iteration by income group
  for(ic in Ic){  
    # Initialize an array to hold all the mode utility data
    ExpUtils.ZiZiMd <- array(0, dim=c(length(Zi), length(Zi), length(Md)), 
                                  dimnames=list(Zi, Zi, Md))
    
    # Populate the array with the mode utility data
    for(md in Md){
      # Load the array of zone to zone utilities and assign to Util.ZiZi
      if(md == "bike"){
        ExpUtil.ZiZi <- exp(BikeAccessCoeff.Pr[pr] * 60 * tripDist.ZiZi / 10)
      }
      else if (md == "walk"){
        ExpUtil.ZiZi <- exp(WalkAccessCoeff.Pr[pr] * 60 * tripDist.ZiZi / 3)
      }
      else {
        #
        ModeUtilObjName <- paste("util", md, ic, pr, sep="")
        ModeUtilFileName <- file.path(INPUT_DIR, "TDM/access", paste(ModeUtilObjName, ".RData", sep=""))
        load(ModeUtilFileName) ; rm(ModeUtilFileName)
        ExpUtil.ZiZi <- get(ModeUtilObjName)
        
        rm(list=ls()[ls()==ModeUtilObjName]) ; rm(ModeUtilObjName)
        dimnames(ExpUtil.ZiZi) <- list(Zi, Zi)        
      }
      
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
      out.dir = file.path(INTERMEDIATE_DIR, 'modeprobs')
      dir.create(out.dir, showWarnings = FALSE)
      out.file = file.path(out.dir, paste(obj.name, ".RData", sep=""))
      save(list=obj.name, file=out.file)
    }
        
    # Load trip distribution matrices for each trip purpose and income group
    DistFileName <- file.path(INPUT_DIR, "TDM/tripdist/", paste(pr, ic, "Dist.RData", sep=""))
    load(DistFileName); rm(DistFileName)
    
    # Get trip matrix 
    TripsObjName <- paste(pr, ic, "Dist", sep="")
    Trips.ZiZi <- get(TripsObjName) ; rm(TripsObjName)
    
    # Define array to store trips by mode
    Trips.ZiZiMd <- array(0, dim=c(length(Zi), length(Zi), length(Md)), 
                              dimnames=list(Zi, Zi, Md))
    # Calculate the trips 
    for(md in Md){  
      TripsMd <- Trips.ZiZi * ModeProbs.ZiZiMd[,,md]
      obj.name <- paste(pr, ic, md, "trips", sep="")
      assign(obj.name, TripsMd)
      
      if (FALSE) {
        out.dir = file.path(INTERMEDIATE_DIR, 'trips')
        dir.create(out.dir, showWarnings = FALSE)
        out.file = file.path(out.dir, paste(obj.name, ".RData", sep=""))
        save(list=obj.name, file=out.file)
      }
      # combine trips into array
      Trips.ZiZiMd[,,md] <- TripsMd; rm(TripsMd)
    }
    
    obj.name <- paste(pr, ic, "Trips.ZiZiMd", sep="")
    assign(obj.name, Trips.ZiZiMd)
    if (SAVE.INTERMEDIARIES) {
      out.dir = file.path(INTERMEDIATE_DIR, 'trips')
      dir.create(out.dir, showWarnings = FALSE)
      out.file = file.path(out.dir, paste(obj.name, ".RData", sep=""))
      save(list=obj.name, file = out.file)
    }
    
    # End loop through income groups
  }
  # End loop through purposes
}

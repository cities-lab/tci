# This script calculates modes probabilities mode trips 

# Define zone abbreviation load("CenRdata\\Zi.RData")


# Define bike and walk access utility coefficients
BikeAccessCoeff.Pr <- c(hbw=-3.217, hbs=-1.839, hbr=-1.839, hbo=-1.839)
WalkAccessCoeff.Pr <- c(hbw=-4.389, hbs=-2.532, hbr=-2.532, hbo=-2.532)

# Load trip distance 
load("data/CenTTime/CenRdata/tripDist.RData")
dimnames(tripDist) <- list(Zi,Zi)
tripDist.ZiZi <- tripDist[Zi,Zi]

#Begin iteration by trip purpose
#-------------------------------

#::
for(pr in Pr){ 
  # load taz index of centers assign to Centers
  CenterFileName <- paste("data/CenTTime/CenRdata/",pr,"ci.RData", sep="")
  load(CenterFileName);  rm(CenterFileName)
  CentersObjName <- paste(pr,"ci", sep="")
  Centers <- get(CentersObjName); rm(CentersObjName)
  
  # Begin iteration by income group
  for(ic in Ic){
    # Load trip distribution matrices for each income group
    DistFileName <- paste("data/CenTTime/CenRdata/tripdist/", pr, ic, "Dist.RData", sep="")
    load(DistFileName); rm(DistFileName)
    
    # Get trip matrix 
    TripsObjName <- paste(pr, ic, "Dist", sep="")
    Trips.ZiZi <- get(TripsObjName) ; rm(TripsObjName)
    
    
    # Initialize an array to hold all the mode utility data           
    ModesExpUtils.ZiZiMd <- array(0, dim=c(length(Zi), length(Zi), length(Md)), 
                                  dimnames=list(Zi, Zi, Md))
    
    # Populate the array with the mode utility data
    for(md in Md){
      
      # Load the array of zone to zone utilities and assign to Util.ZiZi
      if(md == "bike"){
        ExpUtil.ZiZi <- exp(BikeAccessCoeff.Pr[pr] * 60 * tripDist.ZiZi / 10)
      }
      if(md == "walk"){
        ExpUtil.ZiZi <- exp(WalkAccessCoeff.Pr[pr] * 60 * tripDist.ZiZi / 3)
      }
      if((md != "bike") & (md != "walk")) {
        ModeUtilFileName <- paste("data/CenTTime/CenRdata/access/util", md, ic, pr, ".RData", sep="")
        load(ModeUtilFileName) ; rm(ModeUtilFileName)
        ModeUtilObjName <- paste("util", md, ic, pr, sep="")
        ExpUtil.ZiZi <- get(ModeUtilObjName)
        rm(list=ls()[ls()==ModeUtilObjName]) ; rm(ModeUtilObjName)
        dimnames(ExpUtil.ZiZi) <- list(Zi, Zi)
        
      }
      
      # Add the mode matrix to the array
      ModesExpUtils.ZiZiMd[,,md] <- ExpUtil.ZiZi ; rm(ExpUtil.ZiZi)
    }
    
    # Correct problem with utility for short walk and bike trips
    # Setting everything greater than 1 to 1 makes a minimum
    # trip time of approximately X minutes
    ModesExpUtils.ZiZiMd[ModesExpUtils.ZiZiMd > 0.99] <- 0.99
    
    # Calculate the mode probabilities for all modes
    ModeProbs.ZiZiMd <- sweep(ModesExpUtils.ZiZiMd, c(1,2),
                              apply(ModesExpUtils.ZiZiMd, c(1,2), sum), "/")
    assign(paste(pr, ic, "ModeProbs.ZiZiMd", sep=""), ModeProbs.ZiZiMd)
    save(list=paste(pr, ic, "ModeProbs.ZiZiMd", sep=""), file=paste("data/CenTTime/CenRdata/modeprobs/", pr,ic, "ModeProbs.ZiZiMd.RData", sep=""))
    
    # Calculate the trips 
    for(md in Md){
      
      TripsMd <- Trips.ZiZi*ModeProbs.ZiZiMd[,,md]
      assign(paste(pr, ic, md, "trips", sep=""), TripsMd)
      save(list=paste(pr, ic, md, "trips", sep=""),
           file=paste("data/CenTTime/CenRdata/tripbymode/", pr,ic,md, "trips.RData", sep=""))
      
      # combine trips into array 
      
      TotTrips.ZiMd <- array(0, dim=c(length(Zi), length(Md)), dimnames=list(Zi, Md))
      
      TotTrips.ZiMd[,md] <- rowSums(TripsMd[,Centers]) ;rm(TripsMd)
      assign(paste(pr, ic, "TotTrips.ZiMd", sep=""), TotTrips.ZiMd)
      save(list=paste(pr, ic, "TotTrips.ZiMd", sep=""), 
           file = paste("data/CenTTime/results/tripsarray/", pr,ic, "TotTrips.ZiMd.RData", sep=""))
      
    }    
    
    
    # End loop through income groups
  }
  
  
  
  # End loop through purposes
}

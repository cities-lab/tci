# This script calculates travel time cost by convert travel time to cost with different percentage of hourly wage

# define mode cost transformation coefficient
modecosttrans.Md <- c(driveAlone = 0.5, drivePass=0.5, pass=0.35, busWalk=0.35, parkAndRideBus=0.35, bike=0.5, walk=0.5)

#Begin iteration by trip purpose
#-------------------------------

#::
for (pr in Pr) { 
  
  # Begin iteration by income group
  for (ic in Ic) {
    
    # Begin iteration by mode 
    for (md in Md ) {
      
      # load trips matrix

      
      if ((md == "bike")|(md=="walk")) {
        
          # Begin iteration by calculate method 
          for (cm in Cm) {
        
            # load travel time for bike and walk 
            CmTTimeFileName <- paste("results/traveltime/", cm, pr,ic,md,"time.RData", sep="")
            load(CmTTimeFileName); rm(CmTTimeFileName)
            CmTTimeObjName <- paste(cm, pr, ic, md, "time", sep="")
            CmTTime <- get(CmTTimeObjName); rm(CmTTimeObjName)
        
            # calculate minimal travel time cost for bike and walk   
            assign(paste(cm, pr,ic, md, "timecost", sep=""), CmTTime*modecosttrans.Md[md]*24.77/60)
            save(list=paste(cm, pr,ic, md, "timecost", sep=""), file = paste("results/travelcost/", cm, pr,ic, md,"timecost.RData", sep=""))
        
          # End iteration by calculate metho  
          }
        
        # End calculate travel time for walk and bike 
      }
      
      if ((md != "bike") & (md != "walk")) {
        
        # Begin iteration by time period
        for ( tp in Tp){
          
          # Begion iteration by time period 
          for (cm in Cm) {
          
            # load travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
            CmTTimeTpFileName <- paste("results/traveltime/", cm, pr,ic, md,tp,"time.RData", sep="")
            load(CmTTimeTpFileName); rm(CmTTimeTpFileName)
            CmTTimeTpObjName <- paste(cm, pr, ic, md, tp, "time", sep="")
            CmTTimeTp <- get(CmTTimeTpObjName); rm(CmTTimeTpObjName)	
          
            # calculate minimal travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
            assign(paste(cm, pr, ic, md, tp, "timecost", sep=""), CmTTimeTp*modecosttrans.Md[md]*24.77/60)
            save(list=paste(cm,pr,ic, md,tp,"timecost", sep=""), 
                    file = paste("results/travelcost/", cm, pr,ic, md,tp,"timecost.RData", sep=""))
          
           #End iteration by calculate mothod 
          }

          # End loop through time period
        }
        
        # End calculate travel time for driveAlone, drivePass, pass, busWalk, parkAndRideBus
      }
      
      # End loop through mode 
    }
    
    # End loop through income group 
  }
  
  # End loop through purpose
}


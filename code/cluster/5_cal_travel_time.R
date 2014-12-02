# This script calculates minimal travel time and weighted average travel time to centers, 
# converts travel time into monetary cost and combines cost into arrays. 


# define a function of calculate minimal 
min_tt <- function(centers, tt) {
  min_tt <- NULL
  for (zi in 1:dim(tt)[1]) {
    min_tt[zi] <- min(tt[zi,centers], na.rm=TRUE)
  }
  min_tt
}

# define a function to calculate weighted average travel time 
weighted_avg_tt <- function(centers, tt, trips) {
  weighted_avg_tt <- NULL
  for (zi in 1:dim(tt)[1]) {
    trip.sum <- sum(trips[zi, centers])
    weighted_avg_tt[zi] <- sum(tt[zi,centers]*trips[zi,centers], na.rm=TRUE)/trip.sum
  }
  weighted_avg_tt
}

# define a constant for converting time to cost 
cost.conversion <- 24.77/60 

#Begin iteration by trip purpose
#-------------------------------  

#::
for (pr in Pr) { 
  # load taz index of centers assign to Centers
  CenterFileName <- paste("data/CenTTime/CenRdata/",pr,"ci.RData", sep="")
  load(CenterFileName);  rm(CenterFileName)
  CentersObjName <- paste(pr,"ci", sep="")
  Centers <- get(CentersObjName); rm(CentersObjName)
  
  # Begin iteration by income group
  for (ic in Ic) {
    
    # Begin iteration by time period
    for ( tp in Tp){
      TTimecost.ZiMdCm <- array(0, dim=(c(length(Zi),length(Md),length(Cm))),dimnames=list(Zi,Md,Cm))
      
      # Begin iteration by mode 
      for (md in Md ) {
        
        # load trips matrix
        TripsMdFileName <- paste("data/CenTTime/CenRdata/tripbymode/", pr, ic, md, "trips.RData", sep="")
        load(TripsMdFileName); rm(TripsMdFileName)
        TripsMdObjName <- paste(pr, ic, md, "trips", sep="")
        TripsMd <- get(TripsMdObjName); rm(TripsMdObjName)
        
        if ((md == "bike")|(md=="walk")) {
          
          # load travel time for bike and walk 
          TTimeFileName <- paste("data/CenTTime/CenRdata/", md, "Time",md, pr, ".RData", sep="")
          load(TTimeFileName); rm(TTimeFileName)
          TTimeObjName <- paste(md,"Time", md, pr, sep="")
          TTime <- get(TTimeObjName); rm(TTimeObjName)
          
          # calculate and save minimal travel time for bike and walk 
          minTTime <- min_tt(Centers, TTime)
          assign(paste("min", pr, ic,md, "time", sep=""), minTTime)
          save(list=paste("min", pr, ic,md, "time", sep=""), file = paste("data/CenTTime/results/traveltime/", "min", pr,ic,md,"time.RData", sep=""))
          
          # calculate and save minimal travel time cost for bike and walk					
          minTTimeCost <- minTTime*modecosttrans.Md[md]*cost.conversion
          assign(paste("min", pr,ic, md, "timecost", sep=""), minTTimeCost)
          save(list=paste("min", pr,ic, md, "timecost", sep=""), file = paste("data/CenTTime/results/travelcost/", "min", pr,ic, md,"timecost.RData", sep=""))
          
          # combine cost into array 
          TTimecost.ZiMdCm[,md,"min"] <- minTTimeCost       
          remove(minTTime,minTTimeCost)
          
          # calculate and save weighted average travel time for bike and walk 
          weightedTTime <- weighted_avg_tt(Centers, TTime, TripsMd)
          assign(paste("weighted", pr, ic,md, "time", sep=""), weightedTTime)
          save(list=paste("weighted", pr, ic,md, "time", sep=""), file = paste("data/CenTTime/results/traveltime/", "weighted", pr,ic,md,"time.RData", sep=""))
          
          
          # calculate and save weighted average travel time for bike and walk 
          weightedTTimeCost <- weightedTTime*modecosttrans.Md[md]*cost.conversion
          assign(paste("weighted", pr,ic, md, "timecost", sep=""), weightedTTimeCost)
          save(list=paste("weighted", pr,ic, md, "timecost", sep=""), file = paste("data/CenTTime/results/travelcost/", "weighted", pr,ic, md,"timecost.RData", sep=""))
          
          # combine cost into array 
          TTimecost.ZiMdCm[,md,"weighted"] <- weightedTTimeCost
          
          remove(weightedTTime,weightedTTimeCost)
          
          
          
          # End calculate travel time for walk and bike 
        }
        
        if ((md != "bike") & (md != "walk")) {
          
          
          
          # load travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
          TTimeTpFileName <- paste("data/CenTTime/CenRdata/", md, tp, "Time.RData", sep="")
          load(TTimeTpFileName); rm(TTimeTpFileName)
          TTimeTpObjName <- paste(md, tp, "Time", sep = "")
          TTimeTp <- get(TTimeTpObjName); rm(TTimeTpObjName)	
          
          # calculate and save minimal travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
          minTTimeTp <-  min_tt(Centers, TTimeTp)
          assign(paste("min", pr, ic,md,tp, "time", sep=""), minTTimeTp)
          save(list=paste("min",pr,ic,md,tp,"time", sep=""), 
               file = paste("data/CenTTime/results/traveltime/", "min", pr,ic,md,tp,"time.RData", sep=""))
          
          # calculate and save minimal travel time cost for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
          minTTimeCostTp <- minTTimeTp*modecosttrans.Md[md]*cost.conversion
          assign(paste("min", pr,ic, md,tp, "timecost", sep=""), minTTimeCostTp)
          save(list=paste("min", pr,ic,md,tp, "timecost", sep=""), 
               file = paste("data/CenTTime/results/travelcost/", "min", pr,ic, md,tp,"timecost.RData", sep=""))
          
          # combine cost into array 
          TTimecost.ZiMdCm[,md,"min"] <- minTTimeCostTp
          
          remove(minTTimeTp,minTTimeCostTp)
          
          # calculate and save weighted average travel time for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
          weightedTTimeTp <-  weighted_avg_tt(Centers, TTimeTp,TripsMd)
          assign(paste("weighted", pr, ic,md,tp, "time", sep=""), weightedTTimeTp)
          save(list=paste("weighted",pr,ic, md, tp, "time", sep=""), 
               file = paste("data/CenTTime/results/traveltime/", "weighted", pr,ic,md,tp,"time.RData", sep=""))
          
          # calculate and save weighted average travel time cost for md: driveAlone, drivePass, pass, busWalk, parkAndRideBus
          weightedTTimeCostTp <- weightedTTimeTp*modecosttrans.Md[md]*cost.conversion
          assign(paste("weighted", pr,ic, md,tp, "timecost", sep=""), weightedTTimeCostTp)
          save(list=paste("weighted", pr,ic,md,tp, "timecost", sep=""), 
               file = paste("data/CenTTime/results/travelcost/", "weighted", pr,ic, md,tp,"timecost.RData", sep=""))
          
          # combine cost into array 
          TTimecost.ZiMdCm[,md,"weighted"] <- weightedTTimeCostTp
          
          remove(weightedTTimeTp,weightedTTimeCostTp)
          
          # End calculate travel time for driveAlone, drivePass, pass, busWalk, parkAndRideBus
        }
        
        # End loop through mode 
      }
      
      assign(paste(pr, ic, tp,"TimeCost.ZiMdCm", sep=""), TTimecost.ZiMdCm)
      save(list=paste(pr,ic,tp, "TimeCost.ZiMdCm", sep=""), 
           file = paste("data/CenTTime/results/costarray/", pr,ic, tp,"TimeCost.ZiMdCm.RData", sep=""))
      
      rm(TTimecost.ZiMdCm)
      
      # End loop through time period
    }
    
    # End loop through income group 
  }
  
  # End loop through purpose
}


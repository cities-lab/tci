## This scripts combine trips and costs of modes into array and aggregate cost

# Combine trips into array by purpose and income 

#:: Begin iteration by trip purpose 
#::
for (pr in Pr) { 
  # load taz index of centers assign to Centers
  CenterFileName <- paste("CenRdata/",pr,"ci.RData", sep="")
  load(CenterFileName);  rm(CenterFileName)
  CentersObjName <- paste(pr,"ci", sep="")
  Centers <- get(CentersObjName); rm(CentersObjName)
  
  # Begin iteration by income group
  for (ic in Ic) {
    
    # Initialize an array to hold total trips to centers of all modes
    TotTrips.ZiMd <- array(0, dim=c(length(Zi), length(Md)), dimnames=list(Zi, Md))
    
    # Begin iteration by mode 
    for (md in Md ) {
      
      
      
      # load trips matrix
      TripsMdFileName <- paste("CenRdata/tripbymode/", pr, ic, md, "trips.RData", sep="")
      load(TripsMdFileName); rm(TripsMdFileName)
      TripsMdObjName <- paste(pr, ic, md, "trips", sep="")
      TripsMd <- get(TripsMdObjName); rm(TripsMdObjName)
      
      
      # cacluate total trips to TAZs of clusters by modes
      TotTrips.ZiMd[,md] <- rowSums(TripsMd[,Centers]) ;rm(TripsMd)
      
      # End loop through mode  
    }
    
    assign(paste(pr, ic, "TotTrips.ZiMd", sep=""), TotTrips.ZiMd)
    save(list=paste(pr, ic, "TotTrips.ZiMd", sep=""), 
         file = paste("results/tripsarray/", pr,ic, "TotTrips.ZiMd.RData", sep=""))
    
    
    rm(TotTrips.ZiMd)
    
    
    
    # End loop by income group 
  }
  
  # End loop through purpose
}



#### combine the cost into arrary by purpose, income, calculate method and time period 
 
#:: Begin iteration by trip purpose 
for (pr in Pr) { 
  
  # Begin iteration by income group
  for (ic in Ic) {
    
    # Begin iteration by calculate method 
    for (cm in Cm) {
      
      # Begin iteration by time period  
      for (tp in Tp) {
      
        # Initialize an array to hold trave time cost of all modes
        TimeCost.ZiMd <- array(0,dim=c(length(Zi), length(Md)), dimnames=list(Zi, Md)) 
    
        # Begin iteration by mode 
        for (md in Md ) {  
            
            if ((md == "bike")|(md=="walk")) {
        
            
            # load travel time cost for bike and walk 
            CmTTimeCostFileName <- paste("results/travelcost/", cm, pr,ic, md,"timecost.RData", sep="")
            load(CmTTimeCostFileName); rm(CmTTimeCostFileName)
            CmTTimeCostObjName <- paste(cm, pr,ic, md, "timecost", sep="")
            CmTTimeCost <- get(CmTTimeCostObjName); rm(CmTTimeCostObjName)
        
            # add travel time cost to array 
            TimeCost.ZiMd[,md] <- CmTTimeCost; rm(CmTTimeCost)
        
            # End add cost to array for bike and walk mode 
            }
      
            if ((md != "bike") & (md != "walk")) {
        
        
            # load travel time cost for bike and walk 
            CmTTimeCostFileName <- paste("results/travelcost/", cm, pr,ic, md,tp,"timecost.RData", sep="")
            load(CmTTimeCostFileName); rm(CmTTimeCostFileName)
            CmTTimeCostObjName <- paste(cm,pr,ic, md,tp,"timecost", sep="")
            CmTTimeCost <- get(CmTTimeCostObjName); rm(CmTTimeCostObjName)
        
            # add cost to array 
            TimeCost.ZiMd[,md] <- CmTTimeCost; rm(CmTTimeCost)
        
            # End add cost to array for driveAlone, drivePass, pass, busWalk, parkAndRideBus
            }
      
          # End loop through mode  
        }
    
    assign(paste(cm,pr, ic, tp,"TimeCost.ZiMd", sep=""), TimeCost.ZiMd)
    save(list=paste(cm,pr,ic,tp, "TimeCost.ZiMd", sep=""), 
         file = paste("results/costarray/", cm, pr,ic, tp,"TimeCost.ZiMd.RData", sep=""))
    
    rm(TimeCost.ZiMd)
    
      
      # End loop by time period 
      }
    
    # End loop by calculate method 
    }
    
    # End loop by income group 
  }
  
  # End loop through purpose
}



#### aggregate the cost into array by purpose, income, calculate method and time period 

# Begin iteration by calculate method 
for (cm in Cm) {
  
  # Begin iteration by time period 
  for (tp in Tp) {
      
    AggCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
    #:: Begin iteration by trip purpose 
      for (pr in Pr) { 
  
          # Begin iteration by income group
          for (ic in Ic) {

        
        
        # load trips array 
        TotTripsArrayFileName <-  paste("results/tripsarray/", pr,ic, "TotTrips.ZiMd.RData", sep="")
        load(TotTripsArrayFileName); rm(TotTripsArrayFileName)
        TotTripsArrayObjName <- paste(pr, ic, "TotTrips.ZiMd", sep="")
        TotTripsArray <- get(TotTripsArrayObjName); rm(TotTripsArrayObjName)
        
        # calculate row sum of trips trip array 
        TotTripsArraySum <- rowSums(TotTripsArray, na.rm=TRUE)
        
        # load travel time cost attay 
        
        TimeCostArrayFileName <- paste("results/costarray/", cm, pr,ic, tp,"TimeCost.ZiMd.RData", sep="")
        load(TimeCostArrayFileName); rm(TimeCostArrayFileName) 
        TimeCostArrayObjName <- paste(cm,pr,ic,tp, "TimeCost.ZiMd", sep="")
        TimeCostArray<- get(TimeCostArrayObjName); rm(TimeCostArrayObjName)
        
        # aggregate cost to array 
        AggCost.Zi <- rowSums(TotTripsArray*TimeCostArray,na.rm=TRUE)/TotTripsArraySum
        
        
        assign(paste(cm,pr,ic, tp,"AggCost.Zi", sep=""), AggCost.Zi)
        save(list=paste(cm,pr,ic,tp, "AggCost.Zi", sep=""), 
             file = paste("results/aggcostcmprictp/", cm, pr,ic, tp,"AggCost.Zi.RData", sep=""))
        
        AggCost.ZiIcPr[,ic,pr] <- AggCost.Zi; 
        
        rm(AggCost.Zi, TotTripsArray, TimeCostArray, TotTripsArraySum)
        
        
    
        # End loop by income group 
        }
  
      # End loop through purpose
    }
    
    assign(paste(cm,tp,"AggCost.ZiIcPr", sep=""), AggCost.ZiIcPr)
    save(list=paste(cm,tp, "AggCost.ZiIcPr", sep=""), 
         file = paste("results/aggcostcmtp/", cm, tp,"AggCost.ZiIcPr.RData", sep=""))
    
    rm(AggCost.ZiIcPr)
    
  # End loop by time period 
  }

# End loop by calculate method 
}

#### combine the cost into array by calculate method and time period 

# Begin iteration by calculate method 
for (cm in Cm) {
  
  # Begin iteration by time period 
  for (tp in Tp) {
    
    AggCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
    
    #:: Begin iteration by trip purpose 
    for (pr in Pr) { 
      
      # Begin iteration by income group
      for (ic in Ic) {
        
        
        # load trips array 
        AggCostFileName <-  paste("results/aggcostcmprictp/", cm, pr,ic, tp,"AggCost.Zi.RData", sep="")
        load(AggCostFileName); rm(AggCostFileName)
        AggCostObjName <- paste(cm,pr,ic,tp, "AggCost.Zi", sep="")
        AggCost.Zi <- get(AggCostObjName); rm(AggCostObjName)
        
        # Add aggregate cost to array 
        AggCost.ZiIcPr[,ic,pr] <- AggCost.Zi; rm(AggCost.Zi)
        
        # End loop through income group     
      }
      
      # End loop through trip purpose 
    }
    assign(paste(cm,tp,"AggCost.ZiIcPr", sep=""), AggCost.ZiIcPr)
    save(list=paste(cm,tp, "AggCost.ZiIcPr", sep=""), 
         file = paste("results/aggcostcmtp/", cm, tp,"AggCost.ZiIcPr.RData", sep=""))
    
    rm(AggCost.ZiIcPr)
    # End iteration by time period 
  }
  
  # End iteration by calculate method
}     

## Combine the number of trips produced by income group and purpose into array 

TripProd.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))

load("CenRdata/tripgen/hbwTripProd.ZiIc.RData")
load("CenRdata/tripgen/hbsTripProd.ZiIc.RData")
load("CenRdata/tripgen/hbrTripProd.ZiIc.RData")
load("CenRdata/tripgen/hboTripProd.ZiIc.RData")

TripProd.ZiIcPr[,,"hbw"] <- hbwTripProd.ZiIc
TripProd.ZiIcPr[,,"hbs"] <- hbsTripProd.ZiIc
TripProd.ZiIcPr[,,"hbr"] <- hbrTripProd.ZiIc
TripProd.ZiIcPr[,,"hbo"] <- hboTripProd.ZiIc

TripProd.ZiIc <-  apply(TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))
TripProd.ZiPr <-  apply(TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))
TripProd.Zi <-  apply(TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))

## aggregate minimal travel time cost during offpeak time by purpose and inccome group

load("results/aggcostcmtp/minoffpeakAggCost.ZiIcPr.RData")

minoffpeakAggCost.ZiIC <- apply(minoffpeakAggCost.ZiIcPr*TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))/TripProd.ZiIc
save(minoffpeakAggCost.ZiIC, file="results/aggcostcmtp/minoffpeakAggCost.ZiIC.RData")

minoffpeakAggCost.ZiPr <- apply(minoffpeakAggCost.ZiIcPr*TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))/TripProd.ZiPr
save(minoffpeakAggCost.ZiPr, file="results/aggcostcmtp/minoffpeakAggCost.ZiPr.RData")

minoffpeakAggCost.Zi <- apply(minoffpeakAggCost.ZiIcPr*TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))/TripProd.Zi
save(minoffpeakAggCost.Zi, file="results/aggcostcmtp/minoffpeakAggCost.Zi.RData")



## aggregate weighted travel time cost during peak time by purpose and inccome group 

load("results/aggcostcmtp/weightedoffpeakAggCost.ZiIcPr.RData")

weightedoffpeakAggCost.ZiIC <- apply(weightedoffpeakAggCost.ZiIcPr*TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))/TripProd.ZiIc
save(weightedoffpeakAggCost.ZiIC, file="results/aggcostcmtp/weightedoffpeakAggCost.ZiIC.RData")

weightedoffpeakAggCost.ZiPr <- apply(weightedoffpeakAggCost.ZiIcPr*TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))/TripProd.ZiPr
save(weightedoffpeakAggCost.ZiPr, file="results/aggcostcmtp/weightedoffpeakAggCost.ZiPr.RData")

weightedoffpeakAggCost.Zi <- apply(weightedoffpeakAggCost.ZiIcPr*TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))/TripProd.Zi
save(weightedoffpeakAggCost.Zi, file="results/aggcostcmtp/weightedoffpeakAggCost.Zi.RData")



## aggregate minimal travel time cost during peak time by purpose and inccome group

load("results/aggcostcmtp/minpeakAggCost.ZiIcPr.RData")

minpeakAggCost.ZiIC <- apply(minpeakAggCost.ZiIcPr*TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))/TripProd.ZiIc
save(minpeakAggCost.ZiIC, file="results/aggcostcmtp/minpeakAggCost.ZiIC.RData")

minpeakAggCost.ZiPr <- apply(minpeakAggCost.ZiIcPr*TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))/TripProd.ZiPr
save(minpeakAggCost.ZiPr, file="results/aggcostcmtp/minpeakAggCost.ZiPr.RData")

minpeakAggCost.Zi <- apply(minpeakAggCost.ZiIcPr*TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))/TripProd.Zi
save(minpeakAggCost.Zi, file="results/aggcostcmtp/minpeakAggCost.Zi.RData")



## aggregate weighted travel time cost during peak time by purpose and inccome group

load("results/aggcostcmtp/weightedpeakAggCost.ZiIcPr.RData")

weightedpeakAggCost.ZiIC <- apply(weightedpeakAggCost.ZiIcPr*TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))/TripProd.ZiIc
save(weightedpeakAggCost.ZiIC, file="results/aggcostcmtp/weightedpeakAggCost.ZiIC.RData")

weightedpeakAggCost.ZiPr <- apply(weightedpeakAggCost.ZiIcPr*TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))/TripProd.ZiPr
save(weightedpeakAggCost.ZiPr, file="results/aggcostcmtp/weightedpeakAggCost.ZiPr.RData")

weightedpeakAggCost.Zi <- apply(weightedpeakAggCost.ZiIcPr*TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))/TripProd.Zi
save(weightedpeakAggCost.Zi, file="results/aggcostcmtp/weightedpeakAggCost.Zi.RData")









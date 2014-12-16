## This script aggregates cost. First steps aggregate cost by modes. Second stepsaggregate costs 
## by purpose and income groupp. 

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
        
        TimeCostArrayFileName <- paste("results/costarray/",pr,ic,tp,"TimeCost.ZiMdCm.RData", sep="")
        load(TimeCostArrayFileName); rm(TimeCostArrayFileName) 
        TimeCostArrayObjName <- paste(pr,ic,tp, "TimeCost.ZiMdCm", sep="")
        TimeCostArray<- get(TimeCostArrayObjName); rm(TimeCostArrayObjName)
        
        # aggregate cost to array 
        AggCost.Zi <- rowSums(TotTripsArray*TimeCostArray[,,cm],na.rm=TRUE)/TotTripsArraySum
        
        assign(paste(cm,pr,ic, tp,"AggCost.Zi", sep=""), AggCost.Zi)
        save(list=paste(cm,pr,ic,tp, "AggCost.Zi", sep=""), 
             file = paste("results/aggcostcmprictp/", cm, pr,ic, tp,"AggCost.Zi.RData", sep=""))
        
        # combine cost into array
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









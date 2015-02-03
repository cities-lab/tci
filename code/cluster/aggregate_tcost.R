## This script aggregates cost by purpose and income group

## Combine the number of trips produced by income group and purpose into array 

TripProd.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
tripgen.dir <- file.path(INPUT_DIR, 'TDM/tripgen')
for (pr in Pr) {
  tripprod.name <- paste(pr, 'TripProd.ZiIc', sep="")
  load(file.path(tripgen.dir, paste(tripprod.name, '.RData', sep="")))
  TripProd.ZiIcPr[, , pr] <- get(tripprod.name)
}

TripProd.ZiIc <-  apply(TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))
TripProd.ZiPr <-  apply(TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))
TripProd.Zi <-  apply(TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))

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
        
        #get trips array 
        TotTripsArray.name <- paste(pr, ic, "TotTrips.ZiMd", sep="")
        if (!in.memory(c(TotTripsArray.name)))
          load(file.path(INTERMEDIATE_DIR, paste("tripsarray/", TotTripsArray.name, ".RData", sep="")))
        TotTripsArray <- get(TotTripsArray.name)
        
        # calculate row sum of trips trip array 
        #TotTripsArraySum <- rowSums(TotTripsArray, na.rm=TRUE)
        
        #get travel time cost attay
        TimeCostArray.name <- paste(pr,ic,tp, "TimeCost.ZiMdCm", sep="")
        if (!in.memory(c(TimeCostArray.name)))
          load(file.path(INTERMEDIATE_DIR, paste("costarray/", TimeCostArray.name, ".RData", sep="")))
        TimeCostArray<- get(TimeCostArray.name)
        
        # aggregate cost weighted by trips
        AggCost.Zi <- weighted.mean(TimeCostArray[, , cm], TotTripsArray)
        #AggCost.name <- paste(cm, pr, ic, tp,"AggCost.Zi", sep="")
        #assign(AggCost.name, AggCost.Zi)
        #if (SAVE.INTERMEDIARIES) {
        #  intm.file <- file.path(INTERMEDIATE_DIR, 'aggcostcmprictp/', paste(AggCost.name, ".RData"))
        #  save(list=AggCost.name, file=intm.file)
        #}
        
        # combine cost into array
        AggCost.ZiIcPr[,ic,pr] <- AggCost.Zi;
        
        rm(AggCost.Zi, TotTripsArray, TimeCostArray, TotTripsArraySum)
        
        # End loop by income group 
      }
      
      # End loop through purpose
    }
    
    AggCost.ZiIcPr.name <- paste(cm, tp,"AggCost.ZiIcPr", sep="")
    assign(AggCost.ZiIcPr.name, AggCost.ZiIcPr)
    if (SAVE.INTERMEDIARIES) {
      intm.file <- file.path(INTERMEDIATE_DIR, 'aggcostCmTp/', paste(AggCost.ZiIcPr.name, ".RData"))
      save(list=AggCost.ZiIcPr.name, file=intm.file)
    }
    
    #rm(AggCost.ZiIcPr)
    
    AggCost.ZiIc.name <- paste(cm, tp, 'AggCost.ZiIc')
    AggCost.ZiIc <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE)) / TripProd.ZiIc
    assign(AggCost.ZiIc.name, AggCost.ZiIc)
    
    AggCost.ZiPr.name <- paste(cm, tp, 'AggCost.ZiPr')
    AggCost.ZiPr <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE)) / TripProd.ZiPr
    assign(AggCost.ZiPr.name, AggCost.ZiPr)
    
    AggCost.Zi.name <- paste(cm, tp, 'AggCost.Zi')
    AggCost.Zi <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE)) / TripProd.Zi
    assign(AggCost.Zi, AggCost.Zi.name)
    
    if (SAVE.INTERMEDIARIES) {
      intm.file <- file.path(INTERMEDIATE_DIR, 'aggcostCmTp/', paste(AggCost.Zi.name, ".RData"))
      save(list=c(AggCost.ZiIc.name, AggCost.ZiPr.name, AggCost.Zi.name), file=intm.file)
    }
  } # End loop by time period
} # End loop by calculate method 

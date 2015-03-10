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
          load(file.path(INTERMEDIATE_DIR, paste("trips/", TotTripsArray.name, ".RData", sep="")))
        TotTripsArray <- get(TotTripsArray.name)
        
        # calculate row sum of trips trip array 
        #TotTripsArraySum <- rowSums(TotTripsArray, na.rm=TRUE)
        
        #get travel time cost attay
        TimeCostArray.name <- paste(pr, ic, tp, "TimeCost.ZiMdCm", sep="")
        if (!in.memory(c(TimeCostArray.name)))
          load(file.path(INTERMEDIATE_DIR, paste("costs/", TimeCostArray.name, ".RData", sep="")))
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
        
        rm(AggCost.Zi, TotTripsArray, TimeCostArray)
        
        
      } # End loop by income group 
      
    } # End loop through purpose
    
    AggCost.ZiIcPr.name <- paste(cm, tp, "AggCost.ZiIcPr", sep="")
    assign(AggCost.ZiIcPr.name, AggCost.ZiIcPr)
    
    output.path <- file.path(OUTPUT_DIR, 'aggcostCmTp/')
    dir.create(output.path, showWarnings = FALSE)
    output.file <- file.path(output.path, paste(AggCost.ZiIcPr.name, ".RData", sep=""))
    save(list=AggCost.ZiIcPr.name, file=output.file)
    
    
    #rm(AggCost.ZiIcPr)
    
    AggCost.ZiIc.name <- paste(cm, tp, 'AggCost.ZiIc', sep="")
    AggCost.ZiIc <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE)) / TripProd.ZiIc
    assign(AggCost.ZiIc.name, AggCost.ZiIc)
    
    AggCost.ZiPr.name <- paste(cm, tp, 'AggCost.ZiPr', sep="")
    AggCost.ZiPr <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE)) / TripProd.ZiPr
    assign(AggCost.ZiPr.name, AggCost.ZiPr)
    
    AggCost.Zi.name <- paste(cm, tp, 'AggCost.Zi', sep="")
    AggCost.Zi <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE)) / TripProd.Zi
    assign(AggCost.Zi.name, AggCost.Zi)
    
    #save output
    output.path <- file.path(OUTPUT_DIR, 'aggcostCmTp/')
    dir.create(output.path, showWarnings = FALSE)
    output.file <- file.path(output.path, paste(AggCost.Zi.name, ".RData", sep=""))
    save(list=c(AggCost.ZiIc.name, AggCost.ZiPr.name, AggCost.Zi.name), file=output.file)
    
  } # End loop by time period
} # End loop by calculate method 


# Weigh travel costs by time of day factors

# Begin iteration by calculation method 
for (cm in Cm) {
  
  AggTpCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
  
  # Begin iteration by trip purposes
  for (pr in Pr) {
    
    peakAggCost.ZiIcPr.name <- paste(cm, "peak", "AggCost.ZiIcPr", sep="")
    load(file.path(OUTPUT_DIR, paste("aggcostCmTp/",peakAggCost.ZiIcPr.name, ".RData", sep="" )))
    peakAggCost.ZiIcPr <- get(peakAggCost.ZiIcPr.name)
    
    offpeakAggCost.ZiIcPr.name <- paste(cm, "offpeak", "AggCost.ZiIcPr", sep="")
    load(file.path(OUTPUT_DIR, paste("aggcostCmTp/",offpeakAggCost.ZiIcPr.name, ".RData", sep="" )))
    offpeakAggCost.ZiIcPr <- get(offpeakAggCost.ZiIcPr.name)
    
    AggTpCost.ZiIc <- peakAggCost.ZiIcPr[ , , pr]*PeakFactor.Pr[pr] + offpeakAggCost.ZiIcPr[ , , pr]*(1-PeakFactor.Pr[pr] )
    AggTpCost.ZiIcPr[, , pr] <- AggTpCost.ZiIc
    
  } # End loop by calculation method
  
  AggTpCost.ZiIcPr.name <- paste(cm, "AggCost.ZiIcPr", sep="")
  assign(AggTpCost.ZiIcPr.name, AggTpCost.ZiIcPr)
  
  output.path <- file.path(OUTPUT_DIR, 'aggcostCm/')
  dir.create(output.path, showWarnings = FALSE)
  output.file <- file.path(output.path, paste(AggTpCost.ZiIcPr.name, ".RData", sep=""))
  save(list=AggTpCost.ZiIcPr.name, file=output.file)
  
  
  AggTpCost.ZiIc.name <- paste(cm, 'AggTpCost.ZiIc', sep="")
  AggTpCost.ZiIc <- apply(AggTpCost.ZiIcPr * TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE)) / TripProd.ZiIc
  assign(AggTpCost.ZiIc.name, AggTpCost.ZiIc)
  
  AggTpCost.ZiPr.name <- paste(cm, 'AggTpCost.ZiPr', sep="")
  AggTpCost.ZiPr <- apply(AggTpCost.ZiIcPr * TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE)) / TripProd.ZiPr
  assign(AggTpCost.ZiPr.name, AggTpCost.ZiPr)
  
  AggTpCost.Zi.name <- paste(cm, 'AggTpCost.Zi', sep="")
  AggTpCost.Zi <- apply(AggTpCost.ZiIcPr * TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE)) / TripProd.Zi
  assign(AggTpCost.Zi.name, AggTpCost.Zi)
  
  #save output
  output.path <- file.path(OUTPUT_DIR, 'aggcostCm/')
  dir.create(output.path, showWarnings = FALSE)
  output.file <- file.path(output.path, paste(AggTpCost.Zi.name, ".RData", sep=""))
  save(list=c(AggTpCost.ZiIc.name, AggTpCost.ZiPr.name, AggTpCost.Zi.name), file=output.file)
  
} # End loop by trip purpose 
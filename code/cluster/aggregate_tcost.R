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
        AggCost.Zi <- matrix(0, length(Zi), 1, dimnames=list(Zi, "cost"))
        
        for (zi in Zi) {
          
          AggCost.Zi[zi,] <- weighted.mean(TimeCostArray[zi, , cm], TotTripsArray[zi,])
          
        }
        
        # AggCost.Zi <- weighted.mean(TimeCostArray[, , cm], TotTripsArray)
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
    dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
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
    dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
    output.file <- file.path(output.path, paste(AggCost.Zi.name, ".RData", sep=""))
    save(list=c(AggCost.ZiIc.name, AggCost.ZiPr.name, AggCost.Zi.name), file=output.file)
    
  } # End loop by time period
} # End loop by calculate method 


#### Weight travel costs by time of day factors and aggregate the cost into array by purpose, income, calculate method 

# read time of day table 
in.file_timeofday <- file.path(INPUT_DIR, 'TDM/time_of_day.csv')

TimeOfDay <- read.table(in.file_timeofday, header = TRUE, sep=",")

TimeOfDay <- TimeOfDay %>%
  filter(direction == "p-a") %>%
  dplyr::select(purpose, mode, AM2)


# Initialize an array to store peak factor
PeakFactor.PrMd <- array(0, dim = c(length(Pr), length(Md)), dimnames=list(Pr,Md))

for (pr in Pr){
  
  for (md in Md) { 
    
    PeakFactor <- TimeOfDay %>%
      filter(purpose == pr, mode==md)%>%
      dplyr::select(AM2)  
    
    PeakFactor.PrMd[pr,md] <- as.numeric(PeakFactor)
    
  }
}


# Begin iteration by calculate method 
for (cm in Cm) {
  
  AggCost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
  
  #:: Begin iteration by trip purpose 
  for (pr in Pr) { 
    
    # Begin iteration by income group
    for (ic in Ic) {
      
      TimeCostArray.ZiMdCm <- array(0, dim = c(length(Zi), length(Md), length(Cm)), dimnames=list(Zi,Md,Cm))
      
      #get trips array 
      TotTripsArray.name <- paste(pr, ic, "TotTrips.ZiMd", sep="")
      if (!in.memory(c(TotTripsArray.name)))
        load(file.path(INTERMEDIATE_DIR, paste("trips/", TotTripsArray.name, ".RData", sep="")))
      TotTripsArray <- get(TotTripsArray.name)
      
      # calculate row sum of trips trip array 
      #TotTripsArraySum <- rowSums(TotTripsArray, na.rm=TRUE)
      
      #get peak travel time cost array
      peakTimeCostArray.name <- paste(pr, ic, "peak", "TimeCost.ZiMdCm", sep="")
      if (!in.memory(c(peakTimeCostArray.name)))
        load(file.path(INTERMEDIATE_DIR, paste("costs/", peakTimeCostArray.name, ".RData", sep="")))
      peakTimeCostArray<- get(peakTimeCostArray.name)
      
      #get offpeak travel time cost array
      offpeakTimeCostArray.name <- paste(pr, ic, "offpeak", "TimeCost.ZiMdCm", sep="")
      if (!in.memory(c(offpeakTimeCostArray.name)))
        load(file.path(INTERMEDIATE_DIR, paste("costs/", offpeakTimeCostArray.name, ".RData", sep="")))
      offpeakTimeCostArray<- get(offpeakTimeCostArray.name)
      
      # Begin iteration by mode
      for (md in Md) {
        
        TimeCostArray.ZiMdCm[,md,] <- peakTimeCostArray[,md,]*PeakFactor.PrMd[pr,md] + offpeakTimeCostArray[,md,]*(1-PeakFactor.PrMd[pr,md])
        
        # End iteration by mode
      }
      
      # save teavel time cost weighted by time of day 
      TimeCost.ZiMdCm.name <- paste(pr, ic, "TimeCost.ZiMdCm", sep="")
      assign(TimeCost.ZiMdCm.name, TimeCostArray.ZiMdCm)
      
      if (SAVE.INTERMEDIARIES) {
        intm.path <- file.path(INTERMEDIATE_DIR, 'costs/')
        dir.create(intm.path, showWarnings = FALSE)
        intm.file <- file.path(intm.path, paste(TimeCost.ZiMdCm.name, ".RData", sep=""))
        save(list=TimeCost.ZiMdCm.name, file=intm.file)
      }
      
      
      # aggregate cost weighted by trips
      AggCost.Zi <- weighted.mean(TimeCostArray.ZiMdCm[, , cm], TotTripsArray)
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
  
  AggCost.ZiIcPr.name <- paste(cm, "AggCost.ZiIcPr", sep="")
  assign(AggCost.ZiIcPr.name, AggCost.ZiIcPr)
  
  output.path <- file.path(OUTPUT_DIR, 'aggcostCm/')
  dir.create(output.path, showWarnings = FALSE)
  output.file <- file.path(output.path, paste(AggCost.ZiIcPr.name, ".RData", sep=""))
  save(list=AggCost.ZiIcPr.name, file=output.file)
  
  
  #rm(AggCost.ZiIcPr)
  
  AggCost.ZiIc.name <- paste(cm, 'AggCost.ZiIc', sep="")
  AggCost.ZiIc <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE)) / TripProd.ZiIc
  assign(AggCost.ZiIc.name, AggCost.ZiIc)
  
  AggCost.ZiPr.name <- paste(cm, 'AggCost.ZiPr', sep="")
  AggCost.ZiPr <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE)) / TripProd.ZiPr
  assign(AggCost.ZiPr.name, AggCost.ZiPr)
  
  AggCost.Zi.name <- paste(cm, 'AggCost.Zi', sep="")
  AggCost.Zi <- apply(AggCost.ZiIcPr * TripProd.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE)) / TripProd.Zi
  assign(AggCost.Zi.name, AggCost.Zi)
  
  #save output
  output.path <- file.path(OUTPUT_DIR, 'aggcostCm/')
  dir.create(output.path, showWarnings = FALSE)
  output.file <- file.path(output.path, paste(AggCost.Zi.name, ".RData", sep=""))
  save(list=c(AggCost.ZiIc.name, AggCost.ZiPr.name, AggCost.Zi.name), file=output.file)
  
} # End loop by calculate method
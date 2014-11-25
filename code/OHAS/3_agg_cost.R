## This scripts aggregate cost by trip purpose and income groups

#### combine the cost into array    
  Cost.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))
    
    #:: Begin iteration by trip purpose 
    for (pr in Pr) { 
      
      # Begin iteration by income group
      for (ic in Ic) {
        
        
        # load trips array 
        CostFileName <-  paste("Rdata/cost/", pr,ic,"cost.RData", sep="")
        load(CostFileName); rm(CostFileName)
        CostObjName <- paste(pr,ic,"cost", sep="")
        Cost <- get(CostObjName); rm( CostObjName)
        rowindex <- c(1:nrow(Cost))
        
        
        Cost.Zi <- matrix(0, nrow=2162, ncol=1)

        
          for (i in rowindex) {
            
            Cost.Zi[Cost[i,1],1] <- Cost[i,2]
            
          }
                
        # Add cost to array 
        
        Cost.ZiIcPr[,ic,pr] <- Cost.Zi; rm(Cost, Cost.Zi)
        
        # End loop through income group     
      }
      
      # End loop through trip purpose 
    }
  
# save cost array 
  save(Cost.ZiIcPr, file = "results/Cost.ZiIcPr.RData" )


#### combine the Trips into array    
Trips.ZiIcPr <- array(0, dim=c(length(Zi), length(Ic), length(Pr)), dimnames=list(Zi,Ic,Pr))

## Define a data frame 
tazindex <- c(1:2162)
htaznum <- c(1:2162)
Tazindex <- data.frame(tazindex ,htaznum)

#:: Begin iteration by trip purpose 
for (pr in Pr) { 
  
  # Begin iteration by income group
  for (ic in Ic) {
    
    
    # load trips array 
    TripsFileName <-  paste("Rdata/trips/", pr,ic,"trips.RData", sep="")
    load(TripsFileName); rm(TripsFileName)
    TripsObjName <- paste(pr,ic,"trips", sep="")
    Trips <- get(TripsObjName); rm(TripsObjName)
    NewTrips <- merge(Trips,Tazindex, by.x="htaz",by.y="tazindex", all.x=TRUE)
    rowindex <- c(1:nrow(NewTrips))
    
    Trips.Zi <- matrix(0, nrow=2162, ncol=1)
    
    
    for (i in rowindex) {
      
      Trips.Zi[NewTrips[i,3],1] <-  NewTrips[i,2]
      
    }
    
    # Add Trips to array 
    
    Trips.ZiIcPr[,ic,pr] <- Trips.Zi; rm(Trips, Trips.Zi)
    
    # End loop through income group     
  }
  
  # End loop through trip purpose 
}

# save Trips array 
save(Trips.ZiIcPr, file = "results/Trips.ZiIcPr.RData" )


# calculate trips by purpose and income 
Trips.ZiIc <-  apply(Trips.ZiIcPr, c(1,2), function(x) sum(x, na.rm=TRUE))
Trips.ZiPr <-  apply(Trips.ZiIcPr, c(1,3), function(x) sum(x, na.rm=TRUE))
Trips.Zi <-  apply(Trips.ZiIcPr, 1, function(x) sum(x, na.rm=TRUE))


## calculate cost by purpose and income groups
Cost.ZiIc <- apply(Trips.ZiIcPr*Cost.ZiIcPr,c(1,2),function(x) sum(x, na.rm=TRUE))/Trips.ZiIc
save(Cost.ZiIc, file="results/Cost.ZiIc.RData")

Cost.ZiPr <- apply(Trips.ZiIcPr*Cost.ZiIcPr,c(1,3),function(x) sum(x, na.rm=TRUE))/Trips.ZiPr
save(Cost.ZiPr, file="results/Cost.ZiPr.RData")

Cost.Zi <- apply(Trips.ZiIcPr*Cost.ZiIcPr,1,function(x) sum(x, na.rm=TRUE))/Trips.Zi
save(Cost.Zi, file="results/Cost.Zi.RData")




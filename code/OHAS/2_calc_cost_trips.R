## This scripts calculate cost and number of trips by income groups and purpose for each HTAZ (TAZ of household)

## Calculate travel time cost for each trip
# create mode transformation coefficient 
mode <- c(1:10,97)
coef <- c(0.5,0.5,0.5,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.5)
modecoef <- data.frame(mode, coef)

## load linkedsubset data 
load("Rdata/linkedsubset.RData")

# merge mode transformation coefficient 
linkedsubset <- merge(linkedsubset, modecoef, by="mode", all.x=TRUE)

# calculate travel time cost 
linkedsubset <- mutate(linkedsubset, cost = coef*24.77*trpdur/60)


## calculate cost and trips by purpose and income
#:: Begin iteration by trip purpose 
for (pr in Pr) { 
  
  # Begin iteration by income group
  for (ic in Ic) { 
    
    data <- filter(dplyr::select(linkedsubset,sampn,perno,plano,cost,htaz,newincome), linkedsubset[,pr]==1&newincome==IncomeCoeff.Ic[ic])
    data <- arrange(data, htaz,sampn, perno,plano)
    cost <- summarise(group_by(data, htaz), avecost = mean(cost, na.rm=TRUE))
    
    assign(paste(pr,ic,"cost",sep=""), cost)
    save(list = paste(pr,ic,"cost",sep=""),file=paste("Rdata/cost/", pr,ic, "cost.RData",sep="")) 
    
    trips <- summarise(group_by(data, htaz), trips.count = n())
    
    assign(paste(pr,ic,"trips",sep=""), trips)
    save(list = paste(pr,ic,"trips",sep=""),file=paste("Rdata/trips/", pr,ic, "trips.RData",sep="")) 
    
    # End loop through income group     
  }
  
  # End loop through trip purpose 
}

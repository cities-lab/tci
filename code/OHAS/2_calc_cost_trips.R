# This scripts calculate cost and number of trips by income groups and purpose for each HTAZ (TAZ of household)

# Calculate travel time cost for each trip
# create mode transformation coefficient
hourly.wage <- 24.77
mode <- c(1:10,97)
VOT <- c(0.5,0.5,0.5,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.5) * hourly.wage
VOT.by.mode <- data.frame(mode, VOT)

## load linkedsubset data 
load("data/linked_trips.RData")

# merge mode transformation coefficient 
linked <- left_join(linked, VOT.by.mode, by="mode")
linked <- mutate(linked, trip.cost=VOT*trpdur/60)

# summarize trip-level travel cost by taz, trip purpose, and income level
cost.htaz.tpurp.inc <- linked %>%
  group_by(htaz, tpurp.ch, inc.level) %>%
  summarise(n = n(),
            cost.min=min(trip.cost, na.rm=T),
            cost.avg=mean(trip.cost, na.rm=T),
            cost.max=max(trip.cost, na.rm=T),
            cost.sd=sd(trip.cost, na.rm=T)
            )
# calculate household-level travel cost
cost.hh <- linked %>%
  group_by(sampn) %>%
  summarise(cost=sum(trip.cost))

# summarize household-level travel cost by taz and/or income level
cost.hh <- left_join(cost.hh, hh, by="sampn")

cost.htaz.inc <- cost.hh %>%
  group_by(htaz, inc.level) %>%
  summarise(n = n(),
            cost.min=min(cost, na.rm=T),
            cost.avg=mean(cost, na.rm=T),
            cost.max=max(cost, na.rm=T),
            cost.sd=sd(cost, na.rm=T)
  )

cost.htaz <- cost.hh %>%
  group_by(htaz) %>%
  summarise(n = n(),
            cost.min=min(cost, na.rm=T),
            cost.avg=mean(cost, na.rm=T),
            cost.max=max(cost, na.rm=T),
            cost.sd=sd(cost, na.rm=T)
  )

cost.all <- cost.hh %>%
  mutate(all=1) %>%
  group_by(all) %>%
  summarise(n = n(),
            cost.min=min(cost, na.rm=T),
            cost.avg=mean(cost, na.rm=T),
            cost.max=max(cost, na.rm=T),
            cost.sd=sd(cost, na.rm=T)
  )

load("data/CommonData/districts.RData")

cost.hh <- left_join(cost.hh, districts, by=c("htaz"="zone"))
cost.distr <- cost.hh %>%
  rename(district.id=ugb) %>%
  group_by(district.id) %>%
  summarise(n = n(),
            cost.min=min(cost, na.rm=T),
            cost.avg=mean(cost, na.rm=T),
            cost.max=max(cost, na.rm=T),
            cost.sd=sd(cost, na.rm=T)
  )

## calculate cost and trips by purpose and income
#:: Begin iteration by trip purpose 
for (pr in Pr) { 
  
  # Begin iteration by income group
  for (ic in Ic) { 
    
    data <- filter(dplyr::select(linkedsubset,sampn,perno,plano,cost,htaz,newincome), linkedsubset[,pr]==1&newincome==IncomeCoeff.Ic[ic])
    data <- arrange(data, htaz,sampn, perno,plano)
    cost <- summarise(group_by(data, htaz), avecost = mean(cost, na.rm=TRUE))
    
    assign(paste(pr,ic,"cost",sep=""), cost)
    save(list = paste(pr,ic,"cost",sep=""),file=paste("data/OHASTTime/Rdata/cost/", pr,ic, "cost.RData",sep="")) 
    
    trips <- summarise(group_by(data, htaz), trips.count = n())
    
    assign(paste(pr,ic,"trips",sep=""), trips)
    save(list = paste(pr,ic,"trips",sep=""),file=paste("data/OHASTTime/Rdata/trips/", pr,ic, "trips.RData",sep="")) 
    
    # End loop through income group     
  }
  
  # End loop through trip purpose 
}

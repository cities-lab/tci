## This scripts calculate cost and number of trips by income groups and purpose for each household TAZ

## Calculate travel time cost for each trip
  # create mode transformation coefficient 
  mode <- c(1:10,97)
  coef <- c(0.5,0.5,0.5,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.5)
  modecoef <- data.frame(mode, coef)
  
  # merge mode transformation coefficient 
  linkedsubset <- merge(linkedsubset, modecoef, by="mode", all.x=TRUE)
   
  # calculate travel time cost 
  linkedsubset$cost <- linkedsubset$coef*24.77*linkedsubset$trpdur/60


## calculate cost and trip for hbw
  ##  subset table for hbw and lowInc
  hbwlowIncdata <- subset(linkedsubset, hbw==1&newincome==1, select=c(sampn,perno,plano,hbw,cost,htaz,newincome))
  hbwlowIncdata <- with(hbwlowIncdata, hbwlowIncdata[order(htaz,sampn, perno,plano),])


  # calculate mean travel time cost by htaz for hbw and lowInc
  hbwlowInccost <- aggregate(hbwlowIncdata$cost, by=list(hbwlowIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbwlowInccost <- setNames(hbwlowInccost, c("htaz","avecost"))
  hbwlowInccost <- with(hbwlowInccost, hbwlowInccost[order(htaz),])
  save(hbwlowInccost, file="Rdata/cost/hbwlowInccost.RData")

  # get the trips for hbw and lowInc
  hbwlowInctrips <- as.data.frame(table(hbwlowIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbwlowInctrips) <- c("htaz", "numb")
  save(hbwlowInctrips, file="Rdata/trips/hbwlowInctrips.RData")

  ##  subset table for hbw and midInc
  hbwmidIncdata <- subset(linkedsubset, hbw==1&newincome==2, select=c(sampn,perno,plano,hbw,cost,htaz,newincome))
  hbwmidIncdata <- with(hbwmidIncdata, hbwmidIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbw and midInc
  hbwmidInccost <- aggregate(hbwmidIncdata$cost, by=list(hbwmidIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbwmidInccost <- setNames(hbwmidInccost, c("htaz","avecost"))
  hbwmidInccost <- with(hbwmidInccost, hbwmidInccost[order(htaz),])
  save(hbwmidInccost, file="Rdata/cost/hbwmidInccost.RData")
  
  # get the trips for hbw and midInc
  hbwmidInctrips <- as.data.frame(table(hbwmidIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbwmidInctrips) <- c("htaz", "numb")
  save(hbwmidInctrips, file="Rdata/trips/hbwmidInctrips.RData")
  
  ##  subset table for hbw and highInc
  hbwhighIncdata <- subset(linkedsubset, hbw==1&newincome==3, select=c(sampn,perno,plano,hbw,cost,htaz,newincome))
  hbwhighIncdata <- with(hbwhighIncdata, hbwhighIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbw and highInc
  hbwhighInccost <- aggregate(hbwhighIncdata$cost, by=list(hbwhighIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbwhighInccost <- setNames(hbwhighInccost, c("htaz","avecost"))
  hbwhighInccost <- with(hbwhighInccost, hbwhighInccost[order(htaz),])
  save(hbwhighInccost, file="Rdata/cost/hbwhighInccost.RData")
  
  # get the trips for hbw and highInc
  hbwhighInctrips <- as.data.frame(table(hbwhighIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbwhighInctrips) <- c("htaz", "numb")
  save(hbwhighInctrips, file="Rdata/trips/hbwhighInctrips.RData")
  
## calculate cost and trip for hbs
  ##  subset table for hbs and lowInc
  hbslowIncdata <- subset(linkedsubset, hbs==1&newincome==1, select=c(sampn,perno,plano,hbs,cost,htaz,newincome))
  hbslowIncdata <- with(hbslowIncdata, hbslowIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbs and lowInc
  hbslowInccost <- aggregate(hbslowIncdata$cost, by=list(hbslowIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbslowInccost <- setNames(hbslowInccost, c("htaz","avecost"))
  hbslowInccost <- with(hbslowInccost, hbslowInccost[order(htaz),])
  save(hbslowInccost, file="Rdata/cost/hbslowInccost.RData")
  
  # get the trips for hbs and lowInc
  hbslowInctrips <- as.data.frame(table(hbslowIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbslowInctrips) <- c("htaz", "numb")
  save(hbslowInctrips, file="Rdata/trips/hbslowInctrips.RData")
  
  ##  subset table for hbs and midInc
  hbsmidIncdata <- subset(linkedsubset, hbs==1&newincome==2, select=c(sampn,perno,plano,hbs,cost,htaz,newincome))
  hbsmidIncdata <- with(hbsmidIncdata, hbsmidIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbs and midInc
  hbsmidInccost <- aggregate(hbsmidIncdata$cost, by=list(hbsmidIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbsmidInccost <- setNames(hbsmidInccost, c("htaz","avecost"))
  hbsmidInccost <- with(hbsmidInccost, hbsmidInccost[order(htaz),])
  save(hbsmidInccost, file="Rdata/cost/hbsmidInccost.RData")
  
  # get the trips for hbs and midInc
  hbsmidInctrips <- as.data.frame(table(hbsmidIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbsmidInctrips) <- c("htaz", "numb")
  save(hbsmidInctrips, file="Rdata/trips/hbsmidInctrips.RData")
  
  ##  subset table for hbs and highInc
  hbshighIncdata <- subset(linkedsubset, hbs==1&newincome==3, select=c(sampn,perno,plano,hbs,cost,htaz,newincome))
  hbshighIncdata <- with(hbshighIncdata, hbshighIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbs and highInc
  hbshighInccost <- aggregate(hbshighIncdata$cost, by=list(hbshighIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbshighInccost <- setNames(hbshighInccost, c("htaz","avecost"))
  hbshighInccost <- with(hbshighInccost, hbshighInccost[order(htaz),])
  save(hbshighInccost, file="Rdata/cost/hbshighInccost.RData")
  
  # get the trips for hbs and highInc
  hbshighInctrips <- as.data.frame(table(hbshighIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbshighInctrips) <- c("htaz", "numb")
  save(hbshighInctrips, file="Rdata/trips/hbshighInctrips.RData")
  
## calculate cost and trip for hbr
  ##  subset table for hbr and lowInc
  hbrlowIncdata <- subset(linkedsubset, hbr==1&newincome==1, select=c(sampn,perno,plano,hbr,cost,htaz,newincome))
  hbrlowIncdata <- with(hbrlowIncdata, hbrlowIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbr and lowInc
  hbrlowInccost <- aggregate(hbrlowIncdata$cost, by=list(hbrlowIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbrlowInccost <- setNames(hbrlowInccost, c("htaz","avecost"))
  hbrlowInccost <- with(hbrlowInccost, hbrlowInccost[order(htaz),])
  save(hbrlowInccost, file="Rdata/cost/hbrlowInccost.RData")
  
  # get the trips for hbr and lowInc
  hbrlowInctrips <- as.data.frame(table(hbrlowIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbrlowInctrips) <- c("htaz", "numb")
  save(hbrlowInctrips, file="Rdata/trips/hbrlowInctrips.RData")
  
  ##  subset table for hbr and midInc
  hbrmidIncdata <- subset(linkedsubset, hbr==1&newincome==2, select=c(sampn,perno,plano,hbr,cost,htaz,newincome))
  hbrmidIncdata <- with(hbrmidIncdata, hbrmidIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbr and midInc
  hbrmidInccost <- aggregate(hbrmidIncdata$cost, by=list(hbrmidIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbrmidInccost <- setNames(hbrmidInccost, c("htaz","avecost"))
  hbrmidInccost <- with(hbrmidInccost, hbrmidInccost[order(htaz),])
  save(hbrmidInccost, file="Rdata/cost/hbrmidInccost.RData")
  
  # get the trips for hbr and midInc
  hbrmidInctrips <- as.data.frame(table(hbrmidIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbrmidInctrips) <- c("htaz", "numb")
  save(hbrmidInctrips, file="Rdata/trips/hbrmidInctrips.RData")
  
  ##  subset table for hbr and highInc
  hbrhighIncdata <- subset(linkedsubset, hbr==1&newincome==3, select=c(sampn,perno,plano,hbr,cost,htaz,newincome))
  hbrhighIncdata <- with(hbrhighIncdata, hbrhighIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbr and highInc
  hbrhighInccost <- aggregate(hbrhighIncdata$cost, by=list(hbrhighIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbrhighInccost <- setNames(hbrhighInccost, c("htaz","avecost"))
  hbrhighInccost <- with(hbrhighInccost, hbrhighInccost[order(htaz),])
  save(hbrhighInccost, file="Rdata/cost/hbrhighInccost.RData")
  
  # get the trips for hbr and highInc
  hbrhighInctrips <- as.data.frame(table(hbrhighIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbrhighInctrips) <- c("htaz", "numb")
  save(hbrhighInctrips, file="Rdata/trips/hbrhighInctrips.RData")
  
## calculate cost and trip for hbo
  ##  subset table for hbo and lowInc
  hbolowIncdata <- subset(linkedsubset, hbo==1&newincome==1, select=c(sampn,perno,plano,hbo,cost,htaz,newincome))
  hbolowIncdata <- with(hbolowIncdata, hbolowIncdata[order(htaz,sampn, perno,plano),])
    
  # calculate mean travel time cost by htaz for hbo and lowInc
  hbolowInccost <- aggregate(hbolowIncdata$cost, by=list(hbolowIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbolowInccost <- setNames(hbolowInccost, c("htaz","avecost"))
  hbolowInccost <- with(hbolowInccost, hbolowInccost[order(htaz),])
  save(hbolowInccost, file="Rdata/cost/hbolowInccost.RData")
  
  # get the trips for hbo and lowInc
  hbolowInctrips <- as.data.frame(table(hbolowIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbolowInctrips) <- c("htaz", "numb")
  save(hbolowInctrips, file="Rdata/trips/hbolowInctrips.RData")
  
  ##  subset table for hbo and midInc
  hbomidIncdata <- subset(linkedsubset, hbo==1&newincome==2, select=c(sampn,perno,plano,hbo,cost,htaz,newincome))
  hbomidIncdata <- with(hbomidIncdata, hbomidIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbo and midInc
  hbomidInccost <- aggregate(hbomidIncdata$cost, by=list(hbomidIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbomidInccost <- setNames(hbomidInccost, c("htaz","avecost"))
  hbomidInccost <- with(hbomidInccost, hbomidInccost[order(htaz),])
  save(hbomidInccost, file="Rdata/cost/hbomidInccost.RData")
  
  # get the trips for hbo and midInc
  hbomidInctrips <- as.data.frame(table(hbomidIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbomidInctrips) <- c("htaz", "numb")
  save(hbomidInctrips, file="Rdata/trips/hbomidInctrips.RData")
  
  ##  subset table for hbo and highInc
  hbohighIncdata <- subset(linkedsubset, hbo==1&newincome==3, select=c(sampn,perno,plano,hbo,cost,htaz,newincome))
  hbohighIncdata <- with(hbohighIncdata, hbohighIncdata[order(htaz,sampn, perno,plano),])
  
  
  # calculate mean travel time cost by htaz for hbo and highInc
  hbohighInccost <- aggregate(hbohighIncdata$cost, by=list(hbohighIncdata$htaz), FUN=mean, na.rm=TRUE)
  hbohighInccost <- setNames(hbohighInccost, c("htaz","avecost"))
  hbohighInccost <- with(hbohighInccost, hbohighInccost[order(htaz),])
  save(hbohighInccost, file="Rdata/cost/hbohighInccost.RData")
  
  # get the trips for hbo and highInc
  hbohighInctrips <- as.data.frame(table(hbohighIncdata[,"htaz"]),stringsAsFactors=FALSE)
  colnames(hbohighInctrips) <- c("htaz", "numb")
  save(hbohighInctrips, file="Rdata/trips/hbohighInctrips.RData")
  
  
  
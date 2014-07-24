#===========
#tci_setup.R
#===========

#:author: Martin Mann
#:date: 1/20/07
#:contact: martin.a.mann@odot.state.or.us
#:copyright: Oregon Department of Transportation
#:license: GPL2


codeLoc<-getwd()
    setwd(paste(codeLoc,"/performancemeasures/input",sep=""))
#Read file with some setup parameters
    Setup.CM<- read.csv("Setup.csv",header=FALSE,as.is=TRUE)
        Scen <- as.numeric(Setup.CM[1,2])                             
        ScenYear <- as.numeric(Setup.CM[2,2])
        ScenDescp <- Setup.CM[3,2]
    remove(Setup.CM)
       
 
 #PERSON TRIPS / AUTO TRIPS
 load(paste(codeLoc,"/peaking/dailyvehicle.RData",sep=""))
 Atrips<-get("dailyvehicle")
 TotalAtrips<- sum(Atrips)
 Ic <- c("lowInc", "midInc", "highInc")
 Pr <- c("hbw", "hbs", "hbr", "hbo")
 TotalTrips<-0
 for(pr in Pr){ 
  load(paste(codeLoc,"/tripgen/",pr,"TripProdAry.RData",sep=""))
  Ptrips<-get(paste(pr,"TripProdAry",sep="")) 
  TotalTrips<-sum(Ptrips)+TotalTrips
 }
  PTripsPerAtrips<-data.frame(t(c(22,"PersonTrips/AutoTrips",round(TotalTrips /TotalAtrips,2))))
  write.table(PTripsPerAtrips,paste(codeLoc,"/performancemeasures/final_output/Scenario",Scen,"_",ScenYear,
      "SummaryPerformanceMeasures.csv",sep=""),append=TRUE,sep=",",row.names=FALSE,col.names=FALSE)

#***************************************************************************************  
  
 #TRANSIT MODE SHARE
 Ic <- c("lowInc", "midInc", "highInc")
 Pr <- c("hbw","hbs","hbr","hbo")
 TransitTotal<-0
 tripsTotal<-0
 TransitPercent.Pr<-c(rep(0,5))
 names(TransitPercent.Pr)<-c(Pr,"AllTrips")
 for(pr in Pr){
    transitTrips.Pr <-0
    totalTrips.Pr <-0
    for(ic in Ic) {
      load(paste(codeLoc,"/modec/",pr,"/",ic,"/peakTrips.RData",sep=""))
      load(paste(codeLoc,"/modec/",pr,"/",ic,"/offPeakTrips.RData",sep=""))
      #Collapse across destinations for current purpose and income
      peakTripsByOrigin <- apply(peakTrips,c(1,3),sum)
      offPeakTripsByOrigin <- apply(offPeakTrips,c(1,3),sum)
      #sum Transit portions or peak and offpeak trips for current purpose and income
      transitTrips.Pr <- sum(peakTripsByOrigin[,4:5]) + sum(offPeakTripsByOrigin[,4:5]) + transitTrips.Pr
      #sum all trips for current purpose and income
      totalTrips.Pr <- sum(peakTripsByOrigin) + sum(offPeakTripsByOrigin) + totalTrips.Pr          
      rm(offPeakTrips)
      rm(peakTrips)
      }
    #Calc percent of Transit Trips for current purpose
    TransitPercent.Pr[pr] <- round((transitTrips.Pr/totalTrips.Pr)*100,1)
    TransitTotal <- transitTrips.Pr + TransitTotal
    tripsTotal <-totalTrips.Pr + tripsTotal 
  }
  TransitPercent.Pr["AllTrips"]<- round((TransitTotal/tripsTotal)*100,2)  
  
  write.table(data.frame(t(TransitPercent.Pr)),paste(codeLoc,"/performancemeasures/final_output/Scenario",Scen,"_",ScenYear,
  "TransitModeShare.csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE) 
  setwd(codeLoc)
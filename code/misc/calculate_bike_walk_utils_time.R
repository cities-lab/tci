# Calculate bike & walk mode utilities by trip purpose
source("code/cluster/settings.R")

# According to Metro's 2013 Trip-Based Travel Demand Model Methodology Report
# HBW
#Bike
#U = exp (-3.03 - 0.8*Cbdist - 3.650*Cbcost + 0.1279*ln(MixTotA) ) 
#
#Walk
#U = exp (-5.07 - 4.307*ln(Wdist) + 0.3345*ln(MixRetP))

#HBS(hop), HBR, HBO
#Bike
#U = exp (-1.26*Shop - 0.11*Rec - 1.49*Oth - 0.9*Nbdist - 0.22*Nbcost ) 
#
#Walk
#U = exp (-2.82*Shop - 1.80*Rec - 2.76*Oth - 2.466*ln(Wdist)+ 0.1248*ln(MixRetP) + 0.5997*LrgHH)
#where
#Cbdist  =  Bicycle Commute Trip Distance
#Cbcost  =  Bicycle Commute Route Attractiveness
#Nbdist  =  Bicycle Non-commute Trip Distance
#Nbcost	=  Bicycle Non-commute Route Attractiveness
#MixTotA  =  Total employment access within ½ mile of attraction zone (see Section A.1.b)
#MixRetP	=  Retail employment access within ½ mile of production zone (see Section A.1.b)
#LrgHH  =  1 if large household (3+ persons)

#Define bike and walk speed
bike.speed <- 10
walk.speed <-3

# Bike
bikeUtil.c  <- read.csv(file.path(INPUT_DIR, 'TDM/mf201.csv'), header=F)  #Utils for commuting trips (HBWork, HBCollege)
bikeUtil.nc <- read.csv(file.path(INPUT_DIR, 'TDM/mf203.csv'), header=F)  #Utils for non-commuting trips
utilbikehbw <- exp( matrix(bikeUtil.c[, 3], nrow=max.taz_id, byrow=TRUE,
                      dimnames=list(Zi, Zi)) )
# utilbikehbw <- acast(bikeUtil.c, V1~V2, value.var="V3") ##safer but slower, can handle non-continous/missing zone ids
dimnames(utilbikehbw) <- list(Zi, Zi)
utilbikehbs <- utilbikehbr <- utilbikehbo <- exp( matrix(bikeUtil.nc[, 3], nrow=max.taz_id, 
                                                    byrow=TRUE, dimnames=list(Zi, Zi)) )
# utilbikehbs <- utilbikehbr <- utilbikehbo <- acast(bikeUtil.nc, V1~V2, value.var="V3") ##safer but slower, can handle non-continous/missing zone ids

# bike travel time
# mf202.csv is the bike commute distance from MetroSkims2010
# mf204.csv is the bike non-commute distance from MetroSkims2010
bikeDist.c <- read.csv(file.path(INPUT_DIR, 'TDM/mf202.csv'), header=FALSE)
bikeDist.nc <- read.csv(file.path(INPUT_DIR, 'TDM/mf204.csv'), header=FALSE)

all.equal(bikeDist.c, bikeDist.nc)  # Commute and noncommute distance are the same
bdist.mx <- matrix(bikeDist.c[, 3], nrow=max.taz_id, byrow=TRUE,
                   dimnames=list(Zi, Zi))
bikeTime <- bdist.mx*60/bike.speed

# Walk  
wdist.df <- read.table(file.path(INPUT_DIR, 'TDM/2010_wdist.csv'), sep=",", row.names=1, skip=1) #distance matrix used for calculating walk time
wdist.mx <- as.matrix(wdist.df, dimnames=list(Zi, Zi))
utilwalkhbw <- exp(-5.07 - 4.307 * log(wdist.mx)) # + 0.3345 * log(MixRetP) 
utilwalkhbs <- exp(-2.82 - 2.466 * log(wdist.mx)) #+ 0.1248*ln(MixRetP) + 0.5997*LrgHH)
utilwalkhbr <- exp(-1.80 - 2.466 * log(wdist.mx)) #+ 0.1248*ln(MixRetP) + 0.5997*LrgHH)
utilwalkhbo <- exp(-2.76 - 2.466 * log(wdist.mx)) #+ 0.1248*ln(MixRetP) + 0.5997*LrgHH)
walkTime <- wdist.mx *60/ walk.speed




# Calculate utility with formulas from 2013 Trip-Based Travel Demand Model Methodology Report 


# Load data 
  
  # Define OpCost 
  OpCost <- c(driveAlone=0.211, drivePass=0.5*0.211, pass=0.5*0.211, busWalk=2.5, parkAndRideBus=2.5)
 
  # driveAlone, drivePass, pass
    # IvTime
    # mf39  	2a2stt	2a2 SOV auto travel time                	2012-04-27 11:26:23 
    IvTimepeakdriveAlone <- read.csv(file.path(INPUT_DIR, "TDM/AutoSkims/mf39.csv"), header=FALSE)
    
    IvTimepeakdriveAlone <- IvTimepeakdrivePass <- IvTimepeakpass <- matrix(IvTimepeakdriveAlone[, 3], nrow=max.taz_id, byrow=TRUE,
                                                                            dimnames=list(Zi, Zi))
    
    
    # WalkTime	=  Walk time, by mode: Drive Alone: vehicle egress at trip end (5 min in CBD, 2 min elsewhere);
    # Shared Ride: Drive Alone walk time plus 5 minutes
    # The CBD of Portland include 35 TAZs:1-26 and 198-206. 
    WalkTimedriveAlone <- matrix(2, nrow=2162, ncol=2162, dimnames=list(Zi, Zi))
    WalkTimedriveAlone[, c(1:26, 198:206)] <- 5
    WalkTimedrivePass <- WalkTimepass <- WalkTimedriveAlone + 5
    
    # Distance
    # mf61  	tdist 	Shortest Path Dist for IntraZonals      	2012-05-01 10:06:05 
    # mf40  	2a2std	2a2 SOV auto travel distance            	2012-04-27 11:26:19 
    # TdistpeakdriveAlone <- read.csv(file.path(INPUT_DIR, "TDM/AutoSkims/mf39.csv"), header=FALSE)
    TdistdriveAlone <- read.csv(file.path(INPUT_DIR, "TDM/AutoSkims/mf61.csv"), header=FALSE)
    TdistdriveAlone <- TdistdrivePass <- Tdistpass <- matrix(TdistdriveAlone[, 3], nrow=max.taz_id, byrow=TRUE,
                                                             dimnames=list(Zi, Zi))


  # busWalk, parkAndRideBus
    # IvTime  
    # mf57	 2biv    	        12-12-04 11:11  22 Bus In-vehicle Time
    
    IvTimepeakBus.df <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf57.csv"), header=FALSE)
    IvTimepeakBus <- matrix(NA, nrow=2162, ncol=2162,  dimnames=list(Zi, Zi))
    
    for (i in c(1:nrow(IvTimepeakBus.df))) {
      
      IvTimepeakBus[IvTimepeakBus.df[i,1], IvTimepeakBus.df[i,2]]<-IvTimepeakBus.df[i,3]
      
    }
    
    IvTimepeakBus[IvTimepeakBus==9999] <- NA
    
    # mf58	 2liv    	        12-12-04 11:11  22 LRT In-vehicle Time 
    IvTimepeakLRT.df <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf58.csv"), header=FALSE)
    IvTimepeakLRT <- matrix(NA, nrow=2162, ncol=2162,  dimnames=list(Zi, Zi))
    
    for (i in c(1:nrow(IvTimepeakLRT.df))) {
      
      IvTimepeakLRT[IvTimepeakLRT.df[i,1], IvTimepeakLRT.df[i,2]]<-IvTimepeakLRT.df[i,3]
      
    }
    
    IvTimepeakLRT[IvTimepeakLRT==9999] <- NA
    
    
    # mf59	 2sciv   	        12-12-04 11:11  22 Streetcar In-vehicle Time            
    IvTimepeakSC.df <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf59.csv"), header=FALSE)
    IvTimepeakSC <- matrix(NA, nrow=2162, ncol=2162,  dimnames=list(Zi, Zi))
    
    for (i in c(1:nrow(IvTimepeakSC.df))) {
      
      IvTimepeakSC[IvTimepeakSC.df[i,1], IvTimepeakSC.df[i,2]]<-IvTimepeakSC.df[i,3]
      
    }
    
    IvTimepeakSC[IvTimepeakSC==9999] <- NA
    
    # mf60	 2riv    	        12-12-04 11:11  22 ComRail In-vehicle Time
    IvTimepeakRail.df <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf60.csv"), header=FALSE)
    IvTimepeakRail <- matrix(NA, nrow=2162, ncol=2162,  dimnames=list(Zi, Zi))
    
    for (i in c(1:nrow(IvTimepeakRail.df))) {
      
      IvTimepeakRail[IvTimepeakRail.df[i,1], IvTimepeakRail.df[i,2]]<-IvTimepeakRail.df[i,3]
      
    }
    
    IvTimepeakRail[IvTimepeakRail==9999] <- NA
    
    # Tranwait1, Tranwait2 
    # mf61	 2wt1    	        12-12-04 11:11  22 Initial Wait Time
    TranWait1peak.df <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf61.csv"), header=FALSE)
    TranWait1peak <- matrix(NA, nrow=2162, ncol=2162,  dimnames=list(Zi, Zi))
    
    for (i in c(1:nrow(TranWait1peak.df))) {
      
      TranWait1peak[TranWait1peak.df[i,1], TranWait1peak.df[i,2]]<-TranWait1peak.df[i,3]
      
    }
    
    TranWait1peak[TranWait1peak==9999] <- NA
    
    # mf62	 2wt2    	        12-12-04 11:12  22 Remaining Wait Time  
    TranWait2peak.df <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf62.csv"), header=FALSE)
    TranWait2peak <- matrix(NA, nrow=2162, ncol=2162,  dimnames=list(Zi, Zi))
    
    for (i in c(1:nrow(TranWait2peak.df))) {
      
      TranWait2peak[TranWait2peak.df[i,1], TranWait2peak.df[i,2]]<-TranWait2peak.df[i,3]
      
    }
    
    TranWait2peak[TranWait2peak==9999] <- NA
    
    
    # WalkTime
    # mf63	 2walk   	        12-12-04 11:12  22 Walk Time  
    WalkTimepeakbusWalk <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf63.csv"), header=FALSE)
    nrow(walkTimepeak) # 4674244
    
    WalkTimepeakbusWalk <- WalkTimepeakparkAndRideBus <- matrix(WalkTimepeakbusWalk[, 3], nrow=max.taz_id, byrow=TRUE,
                                                                dimnames=list(Zi, Zi))
    
    # Tdist 
    # busWalkpeakDistance <- IVbuspeakTime.mx*15.2/60
    # mf61  	tdist 	Shortest Path Dist for IntraZonals      	2012-05-01 10:06:05 
    
    
    
    # TranXfrs
    # mf78	 2brd    	        12-12-04 11:12  22 Boardings     
    boardingspeak <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf78.csv"), header=FALSE) 
    nrow(boardingspeak) # 4674244 
    
    boardingspeak <-  matrix(boardingspeak[, 3], nrow=max.taz_id, byrow=TRUE, dimnames=list(Zi, Zi))
    
    TranXfrs <- boardingspeak + 1 
    
    # TranModc
    # mf06	 2modc      12-12-04 11:12  22 Mode Constant 
    TranModc.df <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf06.csv"), header=FALSE) 
    TranModc <- matrix(NA, nrow=2162, ncol=2162,  dimnames=list(Zi, Zi))
    
    for (i in c(1:nrow(TranModc.df))) {
      
      TranModc[TranModc.df[i,1], TranModc.df[i,2]]<-TranModc.df[i,3]
      
    }
    
    # TranStypc
    # mf04	 2stpc      12-12-04 11:12  22 Stop Type Constant (final avg)
    TranStypc.df <- read.csv(file.path(INPUT_DIR, "TDM/TransitSkims/mf04.csv"), header=FALSE)
    TranStypc <- matrix(NA, nrow=2162, ncol=2162,  dimnames=list(Zi, Zi))
    
    for (i in c(1:nrow(TranStypc.df))) {
      
      TranStypc[TranStypc.df[i,1], TranStypc.df[i,2]]<-TranStypc.df[i,3]
      
    }
 
  
# Calculate utility 
    
for (ic in Ic){
  
  for (pr in Pr) {   
    
    Shop = ifelse(pr =="hbs", 1, 0)
    Rec =  ifelse(pr =="hbr", 1, 0)
    Oth =  ifelse(pr=="hbo", 1, 0) 
    
    if (pr == "hbw") {
      
      
      driveAloneExpUtil <- exp (-0.03608*ivTimepeakdriveAlone - 0.09956*walkTimedriveAlone - 0.6587*LowInc*OpCost["driveAlone"] 
                                - 0.6097*MidInc*OpCost["driveAlone"]  - 0.4029*HighInc*OpCost["driveAlone"]  - 2.169*Cval1 
                                - 0.02914*Cval2 - 1.887*ln(Tdist)) 
      
      drivePassExpUtil <- exp (-2.97 - 0.03608*IvTimepeakdrivePass - 0.09956*WalkTimedrivePass - 0.6587*LowInc*OpCost["drivePass"] - 0.6097*MidInc*OpCost["drivePass"] 
                               - 0.4029*HighInc*OpCost["drivePass"] - 0.8725*Cval1 + 0.5853*LrgHH - 1.887*ln(Tdist))
      
      passExpUtil <- exp (-3.48 - 0.03608*IvTimepeakpass - 0.09956*WalkTimedrivePass - 0.6587*LowInc*OpCost - 0.6097*MidInc*OpCost["pass"] 
                          - 0.4029*HighInc*OpCost["pass"]+ 0.07042*MixTotA - 1.887*ln(Tdist)) 
      
      busWalkExpUtil <- exp (-3.38 + TranModc + TranStypc - 0.03608*(IvTimepeakBus + 0.88*IvTimepeakLRT + IvTimepeakSC + 0.88*IvTimepeakRail) 
                             - 0.0576*TranWait1peak - 0.04002*TranWait2peak - 0.09956*WalkTimepeakbusWalk - 0.3*TranXfrs -1.304*ln(Tdist)
                             - 0.6587*LowInc*OpCost["busWalk"] - 0.6097*MidInc*OpCost["busWalk"] - 0.4029*HighInc*OpCost["busWalk"]
                             + 0.1314* ln(MixRetP) + 0.09828*ln(MixTotA) + 0.2842*Work1 + 1.268*Cval0)
      
                       
      parkAndRideBusExpUtil <- NULL # ??
    } 
    
    else {
      
      driveAloneUtil <-  exp (-0.0215*ivTimepeakdriveAlone - 0.1033*walkTimedriveAlone - 0.4724*LowInc*OpCost["driveAlone"] 
                              -  0.2457*MidInc*OpCost["driveAlone"] - 0.2457*HighInc*OpCost["driveAlone"] - 0.747*ln(Tdist)) 
      
      drivePassExpUtil <- exp (-0.97*Shop - 0.54*Rec - 0.74*Oth - 0.0215*IvTimepeakdrivePass - 0.1033*WalkTimedrivePass - 0.4724*LowInc*OpCost["drivePass"] - 
                                0.2457*MidInc*OpCost["drivePass"] - 0.2457*HighInc*OpCost["drivePass"] -1.51*SingHH + 0.8491* LrgHH 
                               - 0.747*ln(Tdist)) 
      
      passExpUtil <- exp (-1.07*Shop - 0.47*Rec - 1.09*Oth - 0.0215*IvTimepeakpass - 0.1033*WalkTimepass - 0.4724*LowInc*OpCost["pass"] - 
                            0.2457*MidInc*OpCost["pass"] - 0.2457*HighInc*OpCost["pass"] - 1.288*SingHH + 1.307*LrgHH - 0.747*ln(Tdist)) 
      
      busWalkExpUtil <- U = exp (-3.68*Shop - 3.46*Rec - 4.12*Oth + TranModc + TranStypc - 0.0215*(IvTimepeakBus + 0.86*IvTimepeakLRT 
                                + IvTimepeakSC + 0.86*IvTimepeakRail) - 0.06847*TranWait1peak - 0.0524*TranWait2peak - 0.1033*WalkTimepeakbusWalk 
                                - 0.3*TranXfrs - 0.4724*LowInc*OpCost["busWalk"] - 0.2457*MidInc*OpCost["busWalk"] - 0.2457*HighInc*OpCost["busWalk"] 
                                + 0.1664* ln(MixTotA) + 1.971*Cval0 + 1.129*Cval1 + 0.2874*Cval2) 
      
      parkAndRideBusExpUtil <- NULL # ??
      
     
      
    }
    
    assign(paste(pr,ic, sep=""), driveAloneUtil)
    assign(paste(pr,ic, sep=""), drivePassExpUtil)
    assign(paste(pr,ic, sep=""), passExpUtil)
    assign(paste(pr,ic, sep=""), busWalkExpUtil)
    assign(paste(pr,ic, sep=""), parkAndRideBusExpUtil)

  } # End loop of purpose 
  
} # End loop of income groups 
  



 
     
     
     
  
  
  
  
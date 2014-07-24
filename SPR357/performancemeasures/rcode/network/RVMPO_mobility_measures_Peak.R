#=============================
#calculate_mobility_measures.R
#=============================

#:Author: Brian Gregor
#:Contact: brian.j.gregor@odot.state.or.us
#:Date: 09/26/05
#:Revisions: Martin Mann
#:Date: 05/16/06
#:License: GPL2
#-------------------------Folder Setup----------------------------------------------------------
# Top Folder: 
# 1."R" shortcut "Start in" folder
#	    Subfolders:
#       1. Emme2
#			      -must contain emme2ban, PMInput.mac, TripLength.mac
#       2. Input:  
#		       -must contain LinkColNames.txt, Setup.csv
#	      3. Output
#		       -will contain Performance Measures and Link data
#             One subfolder:
#               1. pdf
#                -folder contains output from reports from Access database.  This script does not produce
#                 this output but does
#
#*******************************************************************************************************
# Script writes the output Link and summary data to tables in the database.  The link in the reports to the
# graphs needs to be reestablished before printing if the path has been altered.  After running script,
# open the database and print out each of the reports to Adobe Acrobat or PDF995
#*******************************************************************************************************
#--------------------------------------BRING INPUT CSV's and EMME2 DATA--------------------------------------------
#*******************************************************************************************************
    codeLoc<-getwd()
    setwd(paste(codeLoc,"/performancemeasures/inputs",sep=""))
#Read file with some setup parameters
    Setup.CM<- read.csv("Setup.csv",header=FALSE,as.is=TRUE)
        Scen <- as.numeric(Setup.CM[1,2])
        ScenYear <- as.numeric(Setup.CM[2,2])
        ScenDescp <- Setup.CM[3,2]
        Population <- as.numeric(Setup.CM[4,2])
        VehOcc <- as.numeric(Setup.CM[5,2])
        WorkDays <- as.numeric(Setup.CM[6,2])
        from <- as.numeric(Setup.CM[7,2])
        to <- as.numeric(Setup.CM[8,2])
    remove(Setup.CM)
       
#Read file with select Link locations for trip length.    
    SelectLink.Lk__ <- read.csv("SelectLinks.csv",header=TRUE,as.is=TRUE)
     
#Run Emme2 Macro to create Emme2 Output file
    setwd(paste(codeLoc,"/emme2/",sep=""))
    system(paste("emme2 -m PMInputJem.mac"),
        show.output.on.console=TRUE, invisible=TRUE)

#Read Emme2 network information into R
    Links.Lk__ <- read.table("Emme2_Output.txt", header=TRUE)
    file.remove("Emme2_Output.txt")
        
#Read Emme2 Travel Time network information into R        
     read.emme2 <- function(emme2.file) {
     	  emme2.in <- scan(emme2.file, skip=4, what = list(0, "", 0))
      	time.matrix <- t(matrix(emme2.in[[3]], length(unique(emme2.in[[1]]))))
       	rownames(time.matrix) <- colnames(time.matrix) <- unique(emme2.in[[1]])
	      time.matrix
       }
     Peak.time <<- read.emme2("PeakHourTT.txt")
     Peak.vol <- read.emme2("PeakHourVol.txt")
     file.remove("PeakHourTT.txt")
     file.remove("PeakHourVol.txt")
     remove(read.emme2)
      
#*******************************************************************************************************
#--------------------------------------PREP LINKS.LK__ DATAFRAME--------------------------------------------
#*******************************************************************************************************

      setwd(paste(codeLoc,"/performancemeasures",sep=""))
# Link types classify all functional classes of interest
      Lt <- c(1,2,3,4,5,30)
      names(Lt) <- c("Freeway", "Principal Arterial", "Minor Arterial","Collector", "Local", "Freeway Ramp") 
          
#Manipulate Links.Lk__ dataframe, add row and column names
      Links.Lk__<-Links.Lk__[,1:10]
      colnames(Links.Lk__)<- c("Inode","Jnode","Type","Length","Lanes","Ffs","Adt","Peak","Cap","TravelTime") 
      
#Add V/C ratios for the Peak Hour to Links.Lk__ 
      Links.Lk__$PeakVc <- round(Links.Lk__$Peak / (Links.Lk__$Cap*Links.Lk__$Lanes),2)
      
#Add Adt/C ratios for the Peak Hour to Links.Lk__ 
      Links.Lk__$AdtC <- round(Links.Lk__$Adt / (Links.Lk__$Cap*Links.Lk__$Lanes),1)
      
#Add IncidentDelayRatio to  Links.Lk__
      Links.Lk__$InciDlyRatio <- rep(NA, nrow(Links.Lk__))
      Links.Lk__$InciDlyRatio[Links.Lk__$Type %in% c(1,30)] <- 2.4
      Links.Lk__$InciDlyRatio[Links.Lk__$Type %in% c(2:5)] <- 1.1
      
#Define Function to calculate Peak Hour Delay relative to Free Flow speed (speed limit)
      calcHourlyDelay <- function(Length, Ffs, HourlyVol, Lanes, HourlyCap){
            if(Ffs >= 50){
                   CongDelay <-round(((60 * Length / Ffs) * (2 + (49 * (1 - 1.0168 * (HourlyVol
                   / (Lanes * HourlyCap))) ^ 2 + 1.1735) ^ .5 + 7.1176 * (HourlyVol
                   / (Lanes * HourlyCap)) - 8.083305))-(60 * Length / Ffs),3)
                   CongDelay
            } else {
                   CongDelay <-round(((60 * Length / Ffs) * (2 + (16 * (1 - .8298 * (HourlyVol
                   / (Lanes * HourlyCap))) ^ 2 + 1.361) ^ .5 + 3.3192 * (HourlyVol
                   / (Lanes * HourlyCap)) - 5.1666))-(60 * Length / Ffs),3)
                   CongDelay
            }
      }
#Add Peak Hour Delay relative to FFS to Links.Lk__ 
            Links.Lk__[,"PeakCongDelay"] <-round(apply(Links.Lk__,1,function(x)calcHourlyDelay
            (x["Length"],x["Ffs"],x["Peak"], x["Lanes"], x["Cap"])),5)
            
#Define Function to calculate Peak Hour Delay relative to LOS DE
      calcHourlyDelayDE <- function(CongDelay, Length, Ffs,Lanes){
            VCDE=0.87
            if(Ffs >= 50){
                   CongDelayDE <-round((CongDelay +(60 * Length / Ffs))- ((60 * Length / Ffs) *
                   (2 + (49 * (1 - 1.0168 * VCDE) ^ 2 + 1.1735) ^ .5 + 7.1176 * VCDE - 8.083305)),3)
                   CongDelayDE
            } else {
                   CongDelayDE <-round((CongDelay +(60 * Length / Ffs)) -((60 * Length / Ffs) *
                   (2 + (16 * (1 - .8298 * VCDE) ^ 2 + 1.361) ^ .5 + 3.3192 * VCDE - 5.1666)),3)
                   CongDelayDE
            }
      }             
#Add Peak Hour Delay relative to LOS DE to Links.Lk__ 
            Links.Lk__[,"PeakCongDelayDE"] <-round(apply(Links.Lk__,1,function(x)calcHourlyDelayDE
            (x["PeakCongDelay"],x["Length"], x["Ffs"], x["Lanes"])),5)
            #Remove negative delay (occurs when actual V/C is less than V/C at LOS D/E)
            Links.Lk__[Links.Lk__[,"PeakCongDelayDE"]<0,"PeakCongDelayDE"]<-0          
#
#-------------------------CALCULATE PERFORMANCE MEASURES---------------------------------------------------
#     
#Group Peak Hour V/C by lane miles and Percent VMT
      VCBreaks<-unique(c(seq(0,max(Links.Lk__$PeakVc),0.2),ceiling(max(Links.Lk__$PeakVc)*5)/5))
      VCbyLaneMiles<- tapply(Links.Lk__$Length*Links.Lk__$Lanes,cut(Links.Lk__$PeakVc,VCBreaks,right=FALSE),sum)
      VCbyLaneMiles[is.na(as.matrix(VCbyLaneMiles))] <-  0
      VCbyPercLaneMiles<- round(VCbyLaneMiles/sum(VCbyLaneMiles),2)*100
      VCbyVMT<- tapply(Links.Lk__$Length*Links.Lk__$Peak,cut(Links.Lk__$PeakVc,VCBreaks,right=FALSE),sum)
      VCbyVMT[is.na(as.matrix(VCbyVMT))] <-  0
      VCbyPercVMT<- round(VCbyVMT/sum(VCbyVMT),3)*100
      remove(VCBreaks)    
      
#Group ADT/C by lane miles and Percent VMT
      AdtCBreaks<-unique(c(seq(0,max(Links.Lk__$AdtC),1),ceiling(max(Links.Lk__$AdtC))))
      AdtCbyLaneMiles<- tapply(Links.Lk__$Length*Links.Lk__$Lanes,cut(Links.Lk__$AdtC,br= AdtCBreaks,right=FALSE),sum)
      AdtCbyLaneMiles[is.na(as.matrix(AdtCbyLaneMiles))] <-  0
      AdtCbyPercLaneMiles<- round(AdtCbyLaneMiles/sum(AdtCbyLaneMiles),2)*100
      AdtCbyVMT<- tapply(Links.Lk__$Length*Links.Lk__$Adt,cut(Links.Lk__$AdtC,br= AdtCBreaks,right=FALSE),sum)
      AdtCbyVMT[is.na(as.matrix(AdtCbyVMT))] <-  0
      AdtCbyPercVMT<- round(AdtCbyVMT/sum(AdtCbyVMT),3)*100
      remove(AdtCBreaks)
    
#CALCULATE LANE MILES
      LaneMiles.Lk <- Links.Lk__$Length*Links.Lk__$Lanes
      LaneMiles<-sum(LaneMiles.Lk)
      
#CALCULATE MEAN TRAVEL TIME
      FROM<-as.numeric(rownames(Peak.vol))>=from
      TO  <-as.numeric(rownames(Peak.vol))<=to
      Zones.select<-FROM&TO
      VolumeSum <- sum(Peak.vol[Zones.select,Zones.select])
      PeakVolume <- Peak.vol[Zones.select,Zones.select]
      PeakTime <- Peak.time[Zones.select,Zones.select]
      LinkTravel.time <- PeakTime * (PeakVolume/VolumeSum )
      MeanTravel <- round(sum(LinkTravel.time),1)
      
#CALCULATE VEHICLE MILES TRAVELED
     Vmt.Lk <- Links.Lk__$Length*Links.Lk__$Peak
     Vmt <- round(sum(Vmt.Lk),0)
  
 #CALCULATE VEHICLE HOURS TRAVELED
     Vht.Lk <- Links.Lk__$TravelTime*Links.Lk__$Peak
     Vht <- round(sum(Vht.Lk)/60,0)
#
#-------------------------FIND Average Trip Length FROM EMME2 DATA--------------------
    setwd(paste(codeLoc,"/emme2/",sep=""))
#Run macro, find "BaseYear Average Trip Length"(or "FutureYear Average Trip Length")
      system(paste("emme2 -m TripLenJemnR.mac",Scen,sep=" "),
        show.output.on.console=TRUE, invisible=TRUE)

#Extract Average Trip Length value from emme2 file
      TripLenFile <- scan("TripLength.txt", what=" ", sep="\n")
      TripLenFile<-TripLenFile[5]
      TripLenFile<-unlist(strsplit(TripLenFile," "))
      TripLen<-as.numeric(TripLenFile[length(TripLenFile)])
      file.remove("TripLength.txt")
#
#-------------------------FIND Average SL Link Trip Lengths FROM EMME2 DATA--------------------
#
#Run macro, find "BaseYear Average Trip Length"(or "FutureYear Average Trip Length")

       SLTripLen<-data.frame(1)#,length(SelectLink.Lk__[,1]),2)
       for(k in 1:length(SelectLink.Lk__[,1])){
          fnode = SelectLink.Lk__[k,2]
          tnode = SelectLink.Lk__[k,3]
          system(paste("emme2 -m SLTripLenJemnR.mac",fnode,tnode,Scen,sep=" "),
           show.output.on.console=TRUE, invisible=TRUE)
#Extract Average Trip Length value from emme2 file
          SLTripLenFile <- scan("SLTripLength.txt", what=" ", sep="\n")
          SLTripLenFile<-SLTripLenFile[5]
          SLTripLenFile<-unlist(strsplit(SLTripLenFile," "))
          SLTripLen[k,2]<-as.numeric(SLTripLenFile[length(SLTripLenFile)])
          SLTripLen[k,1] <- paste("SLTripLength ",SelectLink.Lk__[k,1],": ",
            SelectLink.Lk__[k,4]," on ", SelectLink.Lk__[k,5]," between ",SelectLink.Lk__[k,6]," and ",
            SelectLink.Lk__[k,7],sep="")          
          file.remove("SLTripLength.txt")  
      }
      remove(SelectLink.Lk__)
      remove(fnode)
      remove(tnode)
   
#Define functions to calculate measures
#======================================
setwd(paste(codeLoc,"/performancemeasures",sep=""))
#Define a function to calculate recurring vehicle hours of delay (VHD) by link
	#:parameter: Delay.Lk - vector of link delay
	#:parameter: Vol.Lk - vector of traffic volume by network link
	#:return: RecurVhd.Lk - vector of vehicle-hours of recurring delay per link
       calcRecurVhd <- function(Delay.Lk, Vol.Lk){
          RecurVhd.Lk <- Delay.Lk * Vol.Lk / 60
          RecurVhd.Lk
          }
#Define a function to calculate recurring and incident vehicle hours of delay (VHD) by link
	#:parameter: Delay.Lk - vector of link delay
	#:parameter: Vol.Lk - vector of ADT by network link
	#:parameter: InciDlyRatio.Lk - vector of ratios of incident delay to recurring delay
	#:return: TotVhd.Lk - vector of total minutes of recurring and incident delay per link
       calcTotVhd <- function(Delay.Lk, Vol.Lk, InciDlyRatio.Lk){
          TotVhd.Lk <- Delay.Lk * (1 + InciDlyRatio.Lk) * Vol.Lk / 60
          TotVhd.Lk
          }
#Define a function to calculate link vehicle miles traveled (VMT)
	#:parameter: Length.Lk - link length in miles
	#:parameter: Vol.Lk - link volume in vehicles
	#:return: Vmt.Lk - link vehicle miles traveled
      calcVmt <- function(Length.Lk, Adt.Lk){
          Vmt.Lk <- Length.Lk * Adt.Lk
          Vmt.Lk
          }
#Define a function to calculate annual person delay
	#:parameter: TotVhd.Lk - vector of total vehicle hours of delay
	#:parameter: VehOcc - average vehicle occupancy
	#:parameter: WorkDays - number of work days per year
	#:return: TotPhd.Lk - number of annual person hours of delay per year
      calcTotPhd <- function(TotVhd.Lk, VehOcc, WorkDays){
 	        TotPhd.Lk <- TotVhd.Lk * VehOcc * WorkDays
          TotPhd.Lk
          } 
#Define a function to calculate travel time index by link type (freeflow comparison)
	#:parameter: Length.Lk - link length in miles
	#:parameter: Adt.Lk - link traffic volume
	#:parameter: TotCongDelay.Lk - link Congestion Delay (FFS) (in minutes)
	#:parameter: Ffs.Lk - link free flow speed
	#:parameter: Type.Lk - link type
	#:return: AveTri.Lt - average travel rate index by link type
      calcAveTypeTti1 <- function(Length.Lk, Adt.Lk, PeakCongDelay.Lk, Ffs.Lk, InciDlyRatio.Lk, Type.Lk){
          TotalPeakDelay.Lk <- PeakCongDelay.Lk * (1 + InciDlyRatio.Lk)
          FfsTime.Lk <- (60 * Length.Lk / Ffs.Lk)
          LinkTime.Lk <- FfsTime.Lk + TotalPeakDelay.Lk
          Tti.Lk <- LinkTime.Lk / (FfsTime.Lk)
          Vmt.Lk <- calcVmt(Length.Lk, Adt.Lk)
          VmtWtTti.Lk <- Tti.Lk * Vmt.Lk
          SumVmtWtTti.Lt <- tapply(VmtWtTti.Lk, Type.Lk, sum)
          SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
          AveTti.Lt <- SumVmtWtTti.Lt / SumVmt.Lt
          AveTti.Lt
          }
       
#Define a function to calculate travel time index by link type (LOS D/E comparison)
	#:parameter: Length.Lk - link length in miles
	#:parameter: Adt.Lk - link traffic volume
	#:parameter: TotCongDelay.Lk - link Congestion (FFS) Delay (in minutes)
	#:parameter: TotDelayDe.Lk - link Congestion (LOSD/E) Delay (in minutes)
	#:parameter: Ffs.Lk - link free flow speed
	#:parameter: Type.Lk - link type
	#:return: AveTri.Lt - average travel rate index by link type
      calcAveTypeTti2 <- function(Length.Lk, Adt.Lk, PeakDelay.Lk, PeakDelayDE.Lk, Ffs.Lk, InciDlyRatio.Lk, Type.Lk){
	        TotalPeakDelay.Lk <- PeakDelay.Lk * (1 + InciDlyRatio.Lk)
	        FfsTime.Lk <- 60 * Length.Lk / Ffs.Lk
	        RecurCongTime.Lk <- FfsTime.Lk + TotalPeakDelay.Lk
     	    DeTime.Lk <- RecurCongTime.Lk - PeakDelayDE.Lk
     	    TotCongTime.Lk <- FfsTime.Lk + TotalPeakDelay.Lk
     	    Tti.Lk <- TotCongTime.Lk / DeTime.Lk
     	    Vmt.Lk <- calcVmt(Length.Lk, Adt.Lk)
     	    VmtWtTti.Lk <- Tti.Lk * Vmt.Lk
     	    SumVmtWtTti.Lt <- tapply(VmtWtTti.Lk, Type.Lk, sum)
     	    SumVmt.Lt <- tapply(Vmt.Lk, Type.Lk, sum)
     	    AveTti.Lt <- SumVmtWtTti.Lt / SumVmt.Lt
     	    AveTti.Lt
      }
  
#Define a function to calculate the RNCI
	#:parameter: LinkAdt - a vector of link traffic volumes
	#:parameter: LinkLanes - a vector of the number of lanes for each link
	#:return: Rnci - a value of the road network concentration index
      calcRnci <- function(LinkAdt, LinkLanes){
            LaneVol <- sort(LinkAdt / LinkLanes)
            PctLaneVol <- cumsum(LaneVol)/sum(LaneVol)
            PctEqVol <- cumsum(rep(1,length(LaneVol)))/(sum(rep(1,length(LaneVol))))
            Rnci <- (sum(PctEqVol)-sum(PctLaneVol))/sum(PctEqVol)
            Rnci
      }

#Define a function to plot a Lorenz curve
	#:parameter: LinkAdt - a vector of link traffic volumes
	#:parameter: LinkLanes - a vector of the number of lanes for each link
	#:return: none
      plotRnci <- function(LinkAdt, LinkLanes, ...){
            LaneVol <- sort(LinkAdt / LinkLanes)
            Xvals <- 100 * (1:length(LaneVol)/length(LaneVol))
            Yvals <- 100 * cumsum(LaneVol)/sum(LaneVol)
            Rnci <- round(calcRnci(LinkAdt, LinkLanes), 2)
            plot(Xvals, Yvals, type="l", lwd=2,
                 xlab="Percent of Links", ylab="Percent of Lane Volume", ...)
            lines(Xvals, Xvals, lty=2, lwd=2)
            text(0, 80, labels=paste("RNCI =", Rnci), pos=4, cex=1.25)
            legend(0, 100, legend=c("Pct of Total Lane Volume", "Line of Equality"),
                 lty=c(1,2), lwd=2, bty="n")
      }
                 
#Define a function to plot a Lorenz curve
	#:parameter: LinkAdt - a vector of link traffic volumes
	#:parameter: LinkLanes - a vector of the number of lanes for each link
	#:return: none
      plotRnciCap <- function(LinkAdt, LinkLanes, LaneCap, ...){
            LinkCap <- LinkLanes*LaneCap
            LaneVol <- sort(LinkAdt / LinkCap)
            Xvals <- 100 * (1:length(LaneVol)/length(LaneVol))
            Yvals <- 100 * cumsum(LaneVol)/sum(LaneVol)
            Rnci <- round(calcRnci(LinkAdt, LinkCap), 2)
            plot(Xvals, Yvals, type="l", lwd=2,
                 xlab="Percent of Links", ylab="Percent of Lane Volume", ...)
            lines(Xvals, Xvals, lty=2, lwd=2)
            text(0, 80, labels=paste("RNCI =", Rnci), pos=4, cex=1.25)
            legend(0, 100, legend=c("Pct of Total Lane Volume", "Line of Equality"),
                 lty=c(1,2), lwd=2, bty="n")
      }

#Conduct tests of measures
#=========================
      attach(Links.Lk__)

#Calculate average annual delay per person for two reference speeds

      #Restricts Calculations to Freeways, Freeway Ramps, Primary Arterials
      Adt_Art <- Adt[Type %in% c(1,2,30)]
      UmsType <- 1 * (Type %in% c(1,2,30))

      # Freeflow reference speed, all Links
      HourlyVol<-Links.Lk__$Peak
      HourlyDelay<-Links.Lk__$PeakCongDelay

      # Calculate daily vehicle hours of delay
      RecurVhdFrfl_All.Lk <- calcRecurVhd(HourlyDelay, HourlyVol)
      TotVhdFrfl_All.Lk <- calcTotVhd(HourlyDelay, HourlyVol, InciDlyRatio)
     
      # Calculate annual person hours of delay
      RecurAnnPhdFrfl_All.Lk <- calcTotPhd(RecurVhdFrfl_All.Lk, VehOcc, WorkDays)
      TotAnnPhdFrfl_All.Lk <- calcTotPhd(TotVhdFrfl_All.Lk, VehOcc, WorkDays)
     
      # Calculate annual hours of delay per capita
      AnnRecurDlyCapFrfl_All <- round(sum(RecurAnnPhdFrfl_All.Lk) / Population,2)
      AnnTotDlyCapFrfl_All <- round(sum(TotAnnPhdFrfl_All.Lk) / Population,2)

#-------------------------------------------------------------------
     # Freeflow reference speed, freeway and principal arterial links
     HourlyVol<-Links.Lk__[Type %in% c(1,2,30),"Peak"]
     HourlyDelay<-Links.Lk__[Type %in% c(1,2,30),"PeakCongDelay"]
     
     # Calculate daily vehicle hours of delay
     RecurVhdFrfl_Art.Lk <- calcRecurVhd(HourlyDelay, HourlyVol)
     TotVhdFrfl_Art.Lk <- calcTotVhd(HourlyDelay, HourlyVol, InciDlyRatio[Type %in% c(1,2,30)])

     # Calculate annual person hours of delay
     RecurAnnPhdFrfl_Art.Lk <- calcTotPhd(RecurVhdFrfl_Art.Lk, VehOcc, WorkDays)
     TotAnnPhdFrfl_Art.Lk <- calcTotPhd(TotVhdFrfl_Art.Lk, VehOcc, WorkDays)

     # Calculate annual hours of delay per capita
     AnnRecurDlyCapFrfl_Art <- round(sum(RecurAnnPhdFrfl_Art.Lk) / Population,2)
     AnnTotDlyCapFrfl_Art <- round(sum(TotAnnPhdFrfl_Art.Lk) / Population,2)

#-------------------------------------------------------------------
     # Moderate flow reference speed, all links
     HourlyVol<-Links.Lk__[,"Peak"]
     HourlyDelayDE<-Links.Lk__[,"PeakCongDelayDE"]
     
     # Calculate daily vehicle hours of delay
     RecurVhdMdfl_All.Lk <- calcRecurVhd(HourlyDelayDE, HourlyVol)
     TotVhdMdfl_All.Lk <- calcTotVhd(HourlyDelayDE, HourlyVol, InciDlyRatio)

     # Calculate annual person hours of delay
     RecurAnnPhdMdfl_All.Lk <- calcTotPhd(RecurVhdMdfl_All.Lk, VehOcc, WorkDays)
     TotAnnPhdMdfl_All.Lk <- calcTotPhd(TotVhdMdfl_All.Lk, VehOcc, WorkDays)

     # Calculate annual hours of delay per capita
     AnnRecurDlyCapMdfl_All <- round(sum(RecurAnnPhdMdfl_All.Lk) / Population,2)
     AnnTotDlyCapMdfl_All <- round(sum(TotAnnPhdMdfl_All.Lk) / Population,2)
#-------------------------------------------------------------------

     # Moderate flow reference speed, freeway and principal arterial links
     HourlyVol<-Links.Lk__[Type %in% c(1,2,30),"Peak"]
     HourlyDelayDE<-Links.Lk__[Type %in% c(1,2,30),"PeakCongDelayDE"]
     
     # Calculate daily vehicle hours of delay
     RecurVhdMdfl_Art.Lk <- calcRecurVhd(HourlyDelayDE, HourlyVol)
     TotVhdMdfl_Art.Lk <- calcTotVhd(HourlyDelayDE, HourlyVol, InciDlyRatio[Type %in% c(1,2,30)])

     # Calculate annual person hours of delay
     RecurAnnPhdMdfl_Art.Lk <- calcTotPhd(RecurVhdMdfl_Art.Lk, VehOcc, WorkDays)
     TotAnnPhdMdfl_Art.Lk <- calcTotPhd(TotVhdMdfl_Art.Lk, VehOcc, WorkDays)

     # Calculate annual hours of delay per capita
     AnnRecurDlyCapMdfl_Art <- round(sum(RecurAnnPhdMdfl_Art.Lk) / Population,2)
     AnnTotDlyCapMdfl_Art <- round(sum(TotAnnPhdMdfl_Art.Lk) / Population,2)
 #-------------------------------------------------------------------
#Calculate Travel Time Index
     UmsType <- 1 * (Type %in% c(1,2,30))
     
     # calculate TTI for Scenario A freeflow speed, all classes
     TTIFFAll<-round(calcAveTypeTti1(Length, Adt, PeakCongDelay, Ffs, InciDlyRatio, rep(1, length(Length))),2)
     # calculate TTI for Scenario A freeflow speed, freeway and principal arterial
     TTIFFArt<-round(calcAveTypeTti1(Length, Adt, PeakCongDelay, Ffs, InciDlyRatio, UmsType),2)
     # calculate TTI for Scenario A modflow speed, all classes
     TTIDEAll<-round(calcAveTypeTti2(Length, Adt, PeakCongDelay, PeakCongDelayDE, Ffs, InciDlyRatio,  rep(1, length(Length))),2)
     # calculate TTI for Scenario A modflow speed, freeway and principal arterial
     TTIDEArt<-round(calcAveTypeTti2(Length, Adt, PeakCongDelay, PeakCongDelayDE, Ffs, InciDlyRatio,  UmsType),2)

#-------------------------OUTPUT PERFORMANCE MEASURES--------------------    
      
      VC<-data.frame(t(rbind(round(VCbyLaneMiles,0),paste(VCbyPercLaneMiles,"%",sep=""),round(VCbyVMT,0),
      paste(VCbyPercVMT,"%",sep=""))))
      colnames(VC)<-c("VCbyLaneMiles","VCbyPercLaneMiles","VCbyVMT","VCbyPercVMT")
      AC<-data.frame(t(rbind(round(AdtCbyLaneMiles,0),paste(AdtCbyPercLaneMiles,"%",sep=""),round(AdtCbyVMT,0),
      paste(AdtCbyPercVMT,"%",sep=""))))
      colnames(AC)<-c("AdtCbyLaneMiles","AdtCbyPercLaneMiles","AdtCbyVMT","AdtCbyPercVMT")

 #Output Summary Measures
      SummPerfMeas <-c(AnnRecurDlyCapFrfl_All, AnnTotDlyCapFrfl_All, 
      AnnRecurDlyCapFrfl_Art, AnnTotDlyCapFrfl_Art, AnnRecurDlyCapMdfl_All, AnnTotDlyCapMdfl_All, 
      AnnRecurDlyCapMdfl_Art, AnnTotDlyCapMdfl_Art, TTIFFAll, TTIFFArt, TTIDEAll, TTIDEArt, 
      LaneMiles, MeanTravel, Vht, Vmt, TripLen, SLTripLen[,2])

      SummPerfMeasHeaders<-unlist(c(
      "Annual Peak Hour Congestion Delay Per Capita (hrs)FreeFlow_AllLinks", 
      "Annual Peak Hour Total Delay Per Capita (hrs)FreeFlow_AllLinks",
      "Annual Peak Hour Congestion Delay Per Capita (hrs)FreeFlow_Arterials",
      "Annual Peak Hour Total Delay Per Capita (hrs)FreeFlow_Arterials",
      "Annual Peak Hour Congestion Delay Per Capita (hrs)LOS D/E_AllLinks",
      "Annual Peak Hour Congestion Delay Per Capita (hrs)LOS D/E_AllLinks", 
      "Annual Peak Hour Total Delay Per Capita (hrs)LOS D/E_Arterials",
      "Annual Peak Hour Total Delay Per Capita (hrs)LOS D/E_Arterials",
      "Peak Hour Travel Time Index FreeFlow_AllLinks",
      "Peak Hour Travel Time Index FreeFlow_NonArterials",
      "Peak Hour Travel Time Index FreeFlow_Arterials", 
      "Peak Hour Travel Time Index LOS D/E_AllLinks",
      "Peak Hour Travel Time Index LOS D/E_NonArterials",
      "Peak Hour Travel Time Index LOS D/E_Arterials",
      "Lane Miles", "Mean Travel Time_Peak Hour(mins)", 
      "Vehicle Hours Traveled_Peak Hour", 
      "Vehicle Miles Traveled_Peak Hour",
      "Average Peak Hour Trip Length",paste("Peak Hour_",SLTripLen[,1],sep="")))

 #      Scen, ScenYear, ScenDescp, 
      SummPerfMeas<-data.frame(unlist(cbind(SummPerfMeasHeaders,SummPerfMeas)))

#Output Performance Measure Results
      write.table(Links.Lk__,paste(getwd(),"/final_output/Scenario",Scen,"_",ScenYear,"LinkPerformanceMeasures.csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE)
      write.table(SummPerfMeas,paste(getwd(),"/final_output/Scenario",Scen,"_",ScenYear,"SummaryPerformanceMeasures.csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE)
      write.table(VC,paste(getwd(),"/final_output/Scenario",Scen,"_",ScenYear,"VCPerformanceMeasures.csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE)
      write.table(AC,paste(getwd(),"/final_output/Scenario",Scen,"_",ScenYear,"ACPerformanceMeasures.csv",sep=""),sep=",",row.names=TRUE,col.names=TRUE)
      
      detach(Links.Lk__)

#Plot RNCI for each functional class comparing scenarios A and B
#---------------------------------------------------------------
    for(lt in Lt[c(1:6)]){
          FileName <- paste(codeLoc,"/performancemeasures/final_output/Base_RCNI_", names(Lt[lt]), ".emf", sep="")
          win.metafile(FileName, width=10, height=7)
          OldPar <- par(mfrow=c(1,1), oma=c(1,1,4,1))
          LinkAdtAlt <- Links.Lk__$Adt[Links.Lk__$Type == lt]
          Lanes <- Links.Lk__$Lanes[Links.Lk__$Type == lt]
          plotRnci(LinkAdtAlt, Lanes, main="Base Year")
          mtext(paste(names(Lt)[Lt == lt], "Road Network Concentration Index"),
               line=1, outer=TRUE, cex=2)
          par(OldPar)
          dev.off()
          }
#Calculate a composite RNCI based on capacities
#---------------------------------------------
     FileName <-paste(codeLoc,"/performancemeasures/final_output/CompositeBase_RNCI.emf", sep="")
     win.metafile(FileName, width=10, height=7)
     OldPar <- par(mfrow=c(1,1), oma=c(1,1,4,1))
      IsMajor <- Links.Lk__$Type %in% Lt
     plotRnciCap(Links.Lk__$Adt[IsMajor],Links.Lk__$Lanes[IsMajor],Links.Lk__$Cap[IsMajor],
               main="Base Year")
     mtext("Capacity Weighted Average Arterial and Collector\nRoad Network Concentration Index",
          line=0, outer=TRUE, cex=2)
     par(OldPar)
     dev.off()
     
     
Setwd(codeLoc)
 #-----------------------------------------------END-----------------------------------------------------------------

